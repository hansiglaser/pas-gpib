Unit Diagrams;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Math,
  Graphics, VectorialDiagram, FPVectorial, FPCanvas, FPImage,
  Instrument,
  Comparison;

Type

  { TComparisonDiagrams }

  TComparisonDiagrams = class
    FComparison           : TComparisonBase;
    FCoord                : TCoordBase;
    FDiagram              : TVectorialDiagram;
    FLabelFontName        : String;
    FLabelFontSize        : Double;
    FLabel1Indent         : Double;
    FLabel2Indent         : Double;
    FRangePenColor        : TFPColor;
    FRangePenWidth        : Double;
    FRangeBrushColor      : TFPColor;
    FAccuracyPenColor     : TFPColor;
    FAccuracyPenWidth     : Double;
    FAccuracyBrushColor   : TFPColor;
    FTestPointPenColor    : TFPColor;
    FTestPointPenWidth    : Double;
    FTestPointLenDrw      : Double;
    FTestPointPassPenColor: TFPColor;
    FTestPointFailPenColor: TFPColor;
    FResultPenColor       : TFPColor;
    FResultPenWidth       : Double;
    FResultLenDrw         : Double;
    FOvlpPassPenColor     : TFPColor;
    FOvlpPassPenWidth     : Double;
    FOvlpPassBrushColor   : TFPColor;
    FOvlpFailPenColor     : TFPColor;
    FOvlpFailPenWidth     : Double;
    FOvlpFailBrushColor   : TFPColor;
    Constructor Create(AComparison:TComparisonBase);
    Destructor  Destroy; override;
    Procedure SetCoord(ACoord : TCoordBase);
    Function  GetRangesRows : Integer;
    Procedure DrawRanges   (AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
    Function  GetProcedureRows : Integer;
    Procedure DrawProcedure(AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
    Function  GetResultsRows : Integer;
    Procedure DrawResults  (AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
    Function  GetResultComparisonRows : Integer;
    Procedure DrawResultComparison(ASetIdx,ATestPointIdx:Integer; AWidth, AHeight : Double; AZoom:Double; ACanvas : TFPCustomCanvas; Out AOverlapRatio : Double);
  End;

Const
  colLtRed      : TFPColor = (Red: $FFFF; Green: $C000; Blue: $C000; Alpha: alphaOpaque);
  colLtGreen    : TFPColor = (Red: $C000; Green: $FFFF; Blue: $C000; Alpha: alphaOpaque);   // this overwrites and hides the definition in FPImage
  colLtBlue     : TFPColor = (Red: $C000; Green: $C000; Blue: $FFFF; Alpha: alphaOpaque);
  colLtOrange   : TFPColor = (Red: $FFFF; Green: $E000; Blue: $C000; Alpha: alphaOpaque);

Implementation

{ TComparisonDiagrams }

Constructor TComparisonDiagrams.Create(AComparison : TComparisonBase);
Var Coord : TBipolarSemiLogXCoord;
Begin
  inherited Create;
  FComparison := AComparison;

  // prepare default coordinates
  Coord := TBipolarSemiLogXCoord.Create;
  FCoord := Coord;
  Coord.FValXNegMax   := 1E1 * 2.0;
  Coord.FValXNegMin   := 1E-1 * 0.5;
  //Coord.FValXNegMax   := 0.0;
  //Coord.FValXNegMin   := 0.0;
  Coord.FValXPosMin   := 1E-8 * 0.5;
  Coord.FValXPosMax   := 1E3 * 2.0;
  Coord.FValXWithZero := True;
  Coord.FValYMin      := 0.0;
  Coord.FValYMax      := 47.0;
  Coord.FDrwZeroWidth := 20.0;
  FDiagram := TVectorialDiagram.Create(FCoord);

  // defaults for drawing parameters
  FLabelFontName        := 'Arial';
  FLabelFontSize        := 10.0;
  FLabel1Indent         :=  5.0;
  FLabel2Indent         := 20.0;
  FRangePenColor        := colBlack;
  FRangePenWidth        := 1.0;
  FRangeBrushColor      := colLtOrange;
  FAccuracyPenColor     := colBlack;
  FAccuracyPenWidth     := 1.0;
  FAccuracyBrushColor   := colLtBlue;
  FTestPointPenColor    := colRed;
  FTestPointPenWidth    := 1.0;
  FTestPointLenDrw      := 4.0;
  FResultPenColor       := colBlack;
  FResultPenWidth       := 1.0;
  FResultLenDrw         := 4.0;
  FTestPointPassPenColor:= colGreen;
  FTestPointFailPenColor:= colRed;
  FOvlpPassPenColor     := colGreen;
  FOvlpPassPenWidth     := 1.0;
  FOvlpPassBrushColor   := colLtGreen;
  FOvlpFailPenColor     := colRed;
  FOvlpFailPenWidth     := 1.0;
  FOvlpFailBrushColor   := colLtRed;
End;

Destructor TComparisonDiagrams.Destroy;
Begin
  FDiagram.Free;
  FCoord.Free;
  inherited Destroy;
End;

Procedure TComparisonDiagrams.SetCoord(ACoord : TCoordBase);
Begin
  FCoord.Free;
  FCoord := ACoord;
  FDiagram.FCoord := ACoord;
  // TVectorialDiagram is ok if the coordinates are changed later on but before
  // the actual drawing happens.
End;

Function TComparisonDiagrams.GetRangesRows : Integer;
Var NI : Integer;
Begin
  // determine Y-height
  Result := 1;
  if assigned(FComparison) and (Length(FComparison.FInstruments) > 0) then     // draw instruments only if a comparison is loaded
    For NI := 0 to Length(FComparison.FInstruments)-1 do
      Result := Result + Length(FComparison.FInstruments[NI].FRanges[qtDCV]) + 1;  // +1 for instrument name and visual separation
End;

Procedure TComparisonDiagrams.DrawRanges(AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
Var NI,NR,NP : Integer;
    Y        : Double;
    TP       : TTestPoints;
    A        : TValueAccuracyMinMax;
Begin
  // setup diagram
  (FCoord as TBipolarSemiLogXCoord).FValYMax := GetRangesRows * 1.0;
  FDiagram.Resize(AWidth, AHeight, ACanvas);
  // draw axes
  FDiagram.DrawAxes;
  // draw ranges and testpoints
  Y := 1.0;
  if assigned(FComparison) and (Length(FComparison.FInstruments) > 0) then     // draw instruments only if a comparison is loaded
    For NI := 0 to Length(FComparison.FInstruments)-1 do
      Begin
        For NR := 0 to Length(FComparison.FInstruments[NI].FRanges[qtDCV])-1 do
          Begin
            FDiagram.DrawRange(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FResolution, FComparison.FInstruments[NI].FRanges[qtDCV][NR].FMaxValue, Y, 0.8,
              FRangePenColor, psSolid, Round(FRangePenWidth), FRangeBrushColor, bsSolid);
            if Length(FComparison.FInstruments[NI].FTestpoints) > 0 then
              Begin
                // draw testpoints only if if they were generated
                TP := FComparison.FInstruments[NI].FTestpoints[NR];
                For NP := 0 to Length(TP.FValues)-1 do
                  Begin
                    A := TValueAccuracyMinMax(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FAccuracy[0].Apply(TP.FValues[NP]));
                    if (TP.FValues[NP] <> 0.0) and (A.FMin > TBipolarSemiLogXCoord(FDiagram.FCoord).FValXPosMin) then
                      FDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, FAccuracyPenColor, psSolid, Round(FAccuracyPenWidth), FAccuracyBrushColor, bsSolid);
                      // some min/max values are smaller than the smallest decade in the SemiLogY diagram
                    A.Free;
                    FDiagram.DrawSymPlus(TP.FValues[NP], Y, FTestPointLenDrw, FTestPointPenColor, psSolid, Round(FTestPointPenWidth));
                  End;
              End;
            FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel2Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
              FLabelFontName, FLabelFontSize, FloatToStr(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FMaxValue));
            Y := Y + 1.0;
          End;
        FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
          FLabelFontName, FLabelFontSize, FComparison.FInstruments[NI].FName+' '+CInstrumentFunction[FComparison.FInstruments[NI].FFunction]);
        Y := Y + 1.0;
      End;
End;

Function TComparisonDiagrams.GetProcedureRows : Integer;
Var NS,NR : Integer;
Begin
  // determine Y-height
  Result := 1;
  if assigned(FComparison) and assigned(FComparison.FProcedure) then     // draw procedure only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Begin
        For NR := 0 to Length(FComparison.FProcedure.FSets[NS].FRanges)-1 do
          if assigned(FComparison.FProcedure.FSets[NS].FRanges[NR]) then
            Inc(Result);
        Inc(Result);
      End;
End;

Procedure TComparisonDiagrams.DrawProcedure(AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
Var NS,NR,NP : Integer;
    Y        : Double;
    St       : String;
Begin
  // setup diagram
  (FCoord as TBipolarSemiLogXCoord).FValYMax := GetProcedureRows*1.0;
  FDiagram.Resize(AWidth, AHeight, ACanvas);
  // draw axes
  FDiagram.DrawAxes;
  // draw comparison procedure
  Y := 1.0;
  if assigned(FComparison) and assigned(FComparison.FProcedure) then     // draw procedure only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Begin
        For NR := 0 to Length(FComparison.FProcedure.FSets[NS].FRanges)-1 do
          Begin
            if not assigned(FComparison.FProcedure.FSets[NS].FRanges[NR]) then continue;
            FDiagram.DrawRange(FComparison.FProcedure.FSets[NS].FRanges[NR].FResolution, FComparison.FProcedure.FSets[NS].FRanges[NR].FMaxValue, Y, 0.8,
              FRangePenColor, psSolid, Round(FRangePenWidth), FRangeBrushColor, bsSolid);
            St := FComparison.FProcedure.FSets[NS].FComparison.FInstruments[NR].FName+' '+FloatToStr(FComparison.FProcedure.FSets[NS].FRanges[NR].FMaxValue);
            FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel2Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
              FLabelFontName, FLabelFontSize, St);
            Y := Y + 1.0;
          End;
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, FTestPointLenDrw, FTestPointPenColor, psSolid, Round(FTestPointPenWidth));
          End;
        St := 'Set #'+IntToStr(NS)+' to '+FloatToStr(FComparison.FProcedure.FSets[NS].FMaxVal);
        FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
          FLabelFontName, FLabelFontSize, St);
        Y := Y + 1.0;
      End;
//  if assigned(FComparison) and assigned(FComparison.FProcedure) then
//    FComparison.FProcedure.PrintRanges;
End;

Function TComparisonDiagrams.GetResultsRows : Integer;
Var NS : Integer;
Begin
  // determine Y-height
  Result := 1;
  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then     // draw results only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Result := Result + Length(FComparison.FInstruments) + 1;  // +1 for instrument name and visual separation
End;

Procedure TComparisonDiagrams.DrawResults(AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
Var NS,NI,NP  : Integer;
    Y         : Double;
    AllRanges : TInstrumentRanges;
    St        : String;
    A         : TValueAccuracyMinMax;
    T         : TvText;
Begin
  // setup diagram
  (FCoord as TBipolarSemiLogXCoord).FValYMax := GetResultsRows;
  FDiagram.Resize(AWidth, AHeight, ACanvas);
  // draw axes
  FDiagram.DrawAxes;
  // draw comparison procedure
  Y := 1.0;
  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then     // draw results only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Begin
        // determine all active ranges
        AllRanges := FComparison.FProcedure.FSets[NS].GetAllRanges;
        // per instrument: range bar
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          Begin
            FDiagram.DrawRange(AllRanges[NI].FResolution, AllRanges[NI].FMaxValue, Y, 0.8,
              FRangePenColor, psSolid, Round(FRangePenWidth), FRangeBrushColor, bsSolid);
            Y := Y + 1.0;
          End;
        Y := Y - Length(FComparison.FInstruments)*1.0;
        // draw overlap region
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          With FComparison.FProcedure.FSets[NS].FAnalyses[NP] do
            Begin
              if FPass then
                FDiagram.DrawRect(FMaxMin, FMinMax, Y-0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
                  FOvlpPassPenColor, psSolid, Round(FOvlpPassPenWidth), FOvlpPassBrushColor, bsSolid)
              else
                FDiagram.DrawRect(FMinMax, FMaxMin, Y-0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
                  FOvlpFailPenColor, psSolid, Round(FOvlpFailPenWidth), FOvlpFailBrushColor, bsSolid);
            End;
        // draw measurement results
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          Begin
            // FMeasurements[InstrumentIdx][TestPointIdx]
            For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
              Begin
                A := FComparison.FProcedure.FSets[NS].FMeasurements[NI][NP].FValue as TValueAccuracyMinMax;
                //if (FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP] <> 0.0) {and (A.FMin > TBipolarSemiLogXCoord(FResultsDiagram.FCoord).FValXPosMin)} then
                  FDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, FAccuracyPenColor, psSolid, Round(FAccuracyPenWidth), FAccuracyBrushColor, bsSolid);
                  // some min/max values are smaller than the smallest decade in the SemiLogY diagram
                FDiagram.DrawSymPlus(A.FValue, Y, FResultLenDrw, FResultPenColor, psSolid, Round(FResultPenWidth));
              End;
            Y := Y + 1.0;
          End;
        // draw testpoints in green or red as reference
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            if FComparison.FProcedure.FSets[NS].FAnalyses[NP].FPass then
              FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, FTestPointLenDrw, FTestPointPassPenColor, psSolid, Round(FTestPointPenWidth))
            else
              FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, FTestPointLenDrw, FTestPointFailPenColor, psSolid, Round(FTestPointPenWidth));
          End;
        Y := Y - Length(FComparison.FInstruments)*1.0;
        // per instrument: text (on top)
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          Begin
            St := FComparison.FInstruments[NI].FName+' '+FloatToStr(AllRanges[NI].FMaxValue);
            T := FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel2Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.4, 0.0,
              FLabelFontName, FLabelFontSize, St, true);  // don't add yet
            FDiagram.Rectangle(
              FDiagram.FDiagBox.Left+FLabel2Indent-FLabelFontSize*0.15,                               FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.60,
              FDiagram.FDiagBox.Left+FLabel2Indent+FLabelFontSize*0.15+FDiagram.EstimateTextWidth(T), FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)+FLabelFontSize*0.45,
              colBlack, psClear, 1, FPColor($FFFF, $FFFF, $FFFF, $B000), bsSolid);
            FDiagram.FVecPage.AddEntity(T);
            Y := Y + 1.0;
          End;
        St := 'Set #'+IntToStr(NS)+' to '+FloatToStr(FComparison.FProcedure.FSets[NS].FMaxVal);
        FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0, FLabelFontName, FLabelFontSize, St);
        Y := Y + 1.0;
      End;
//  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then
//    Begin
//      FComparison.FProcedure.PrintMeasurementsByInstrument;
//      FComparison.FProcedure.PrintMeasurementsByTestPoint;
//    End;
End;

Function TComparisonDiagrams.GetResultComparisonRows : Integer;
Begin
  Result := Length(FComparison.FInstruments) + 1;
End;

Procedure TComparisonDiagrams.DrawResultComparison(ASetIdx, ATestPointIdx : Integer; AWidth, AHeight : Double; AZoom : Double; ACanvas : TFPCustomCanvas; Out AOverlapRatio : Double);
Var NI       : Integer;
    Analysis : TMeasurementAnalysis;
    MinMin,
    MaxMax   : Double;
    A        : TValueAccuracyMinMax;
    Y        : Double;
    T        : TvText;
Begin
  // setup diagram
  (FCoord as TLinearCoord).FValYMin := 0.0;
  (FCoord as TLinearCoord).FValYMax := GetResultComparisonRows*1.0;
  Analysis := FComparison.FProcedure.FSets[ASetIdx].FAnalyses[ATestPointIdx];
  AOverlapRatio := abs(Analysis.FMaxMin-Analysis.FMinMax)/(Analysis.FMaxMax-Analysis.FMinMin);
  // ensure the nominal testpoint is included
  MinMin := min(Analysis.FMinMin, FComparison.FProcedure.FSets[ASetIdx].FTestPoints.FValues[ATestPointIdx]);
  MaxMax := max(Analysis.FMaxMax, FComparison.FProcedure.FSets[ASetIdx].FTestPoints.FValues[ATestPointIdx]);
  if AZoom = 0.0 then   // use 0.0 because this is exact, 1.0 would be more difficult to get exact
    Begin
      (FCoord as TLinearCoord).FValXMin := MinMin - (MaxMax-MinMin)*0.25;
      (FCoord as TLinearCoord).FValXMax := MaxMax + (MaxMax-MinMin)*0.05;
    End
  else
    Begin
      (FCoord as TLinearCoord).FValXMin := (Analysis.FMaxMin+Analysis.FMinMax)*0.5 - 1.0/AZoom*0.5*abs(MaxMax-MinMin);//abs(Analysis.FMaxMin-Analysis.FMinMax);
      (FCoord as TLinearCoord).FValXMax := (Analysis.FMaxMin+Analysis.FMinMax)*0.5 + 1.0/AZoom*0.5*abs(MaxMax-MinMin);//abs(Analysis.FMaxMin-Analysis.FMinMax);
    End;
  FDiagram.Resize(AWidth, AHeight, ACanvas);
  // draw axes
  FDiagram.DrawAxes;
  Y := 1.0;
  // draw overlap region
  if Analysis.FPass then
    FDiagram.DrawRect(Analysis.FMaxMin, Analysis.FMinMax, Y-0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
      FOvlpPassPenColor, psSolid, Round(FOvlpPassPenWidth), FOvlpPassBrushColor, bsSolid)
  else
    FDiagram.DrawRect(Analysis.FMinMax, Analysis.FMaxMin, Y+0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
      FOvlpFailPenColor, psSolid, Round(FOvlpFailPenWidth), FOvlpFailBrushColor, bsSolid);
  // draw comparison procedure
  For NI := 0 to Length(FComparison.FInstruments)-1 do
    Begin
      A := FComparison.FProcedure.FSets[ASetIdx].FMeasurements[NI][ATestPointIdx].FValue as TValueAccuracyMinMax;
      FDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, FAccuracyPenColor, psSolid, Round(FAccuracyPenWidth), FAccuracyBrushColor, bsSolid);
      FDiagram.DrawSymPlus(A.FValue, Y, FResultLenDrw, FResultPenColor, psSolid, Round(FResultPenWidth));
      T := FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.4, 0.0,
              FLabelFontName, FLabelFontSize, FComparison.FInstruments[NI].FName, true);  // don't add yet
      FDiagram.Rectangle(
        FDiagram.FDiagBox.Left+FLabel1Indent-FLabelFontSize*0.15,                               FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.60,
        FDiagram.FDiagBox.Left+FLabel1Indent+FLabelFontSize*0.15+FDiagram.EstimateTextWidth(T), FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)+FLabelFontSize*0.45,
        colBlack, psClear, 1, FPColor($FFFF, $FFFF, $FFFF, $B000), bsSolid);
      FDiagram.FVecPage.AddEntity(T);
      Y := Y + 1.0;
    End;
  // draw testpoints in green or red as reference in the top row
  if Analysis.FPass then
    FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[ASetIdx].FTestPoints.FValues[ATestPointIdx], Y - 1.0, FTestPointLenDrw, FTestPointPassPenColor, psSolid, Round(FTestPointPenWidth))
  else
    FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[ASetIdx].FTestPoints.FValues[ATestPointIdx], Y - 1.0, FTestPointLenDrw, FTestPointFailPenColor, psSolid, Round(FTestPointPenWidth))
End;

End.

