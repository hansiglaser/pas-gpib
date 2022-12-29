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
    FCoord                : TBipolarSemiLogXBase;
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
    FTestPointRefPenColor : TFPColor;
    FResultPenColor       : TFPColor;
    FResultPenWidth       : Double;
    FResultLenDrw         : Double;
    FOvlpOkPenColor       : TFPColor;
    FOvlpOkPenWidth       : Double;
    FOvlpOkBrushColor     : TFPColor;
    FOvlpNOkPenColor      : TFPColor;
    FOvlpNOkPenWidth      : Double;
    FOvlpNOkBrushColor    : TFPColor;
    Constructor Create(AComparison:TComparisonBase);
    Destructor  Destroy; override;
    Procedure DrawRanges   (AWidth, AHeight : Integer; ACanvas : TFPCustomCanvas);
    Procedure DrawProcedure(AWidth, AHeight : Integer; ACanvas : TFPCustomCanvas);
    Procedure DrawResults  (AWidth, AHeight : Integer; ACanvas : TFPCustomCanvas);
  End;

Const
  colLtRed      : TFPColor = (Red: $FFFF; Green: $C000; Blue: $C000; Alpha: alphaOpaque);
  colLtGreen    : TFPColor = (Red: $C000; Green: $FFFF; Blue: $C000; Alpha: alphaOpaque);   // this overwrites and hides the definition in FPImage
  colLtBlue     : TFPColor = (Red: $C000; Green: $C000; Blue: $FFFF; Alpha: alphaOpaque);
  colLtOrange   : TFPColor = (Red: $FFFF; Green: $E000; Blue: $C000; Alpha: alphaOpaque);

Implementation

{ TComparisonDiagrams }

Constructor TComparisonDiagrams.Create(AComparison : TComparisonBase);
Begin
  inherited Create;
  FComparison := AComparison;

  // prepare coordinates
  FCoord := TBipolarSemiLogXBase.Create;
  FCoord.FValXNegMax   := 1E1 * 2.0;
  FCoord.FValXNegMin   := 1E-1 * 0.5;
  //FCoord.FValXNegMax   := 0.0;
  //FCoord.FValXNegMin   := 0.0;
  FCoord.FValXPosMin   := 1E-8 * 0.5;
  FCoord.FValXPosMax   := 1E3 * 2.0;
  FCoord.FValXWithZero := True;
  FCoord.FValYMin      := 0.0;
  FCoord.FValYMax      := 47.0;
  FCoord.FDrwZeroWidth := 20.0;
  FDiagram := TVectorialDiagram.Create(FCoord);
  // TODO: how to make FCoord changable to linear or so?
  //  - TVectorialDiagram would be ok if it is changed later on but before the actual drawing happens
  //  - perhaps make a setter method SetCoord, which Free()s the existing one, and also updates it in FDiagram

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
  FTestPointRefPenColor := colDkGreen;
  FOvlpOkPenColor       := colGreen;
  FOvlpOkPenWidth       := 1.0;
  FOvlpOkBrushColor     := colLtGreen;
  FOvlpNOkPenColor      := colRed;
  FOvlpNOkPenWidth      := 1.0;
  FOvlpNOkBrushColor    := colLtRed;
End;

Destructor TComparisonDiagrams.Destroy;
Begin
  FDiagram.Free;
  FCoord.Free;
  inherited Destroy;
End;

Procedure TComparisonDiagrams.DrawRanges(AWidth, AHeight : Integer; ACanvas : TFPCustomCanvas);
Var NI,NR,NP : Integer;
    Y        : Double;
    TP       : TTestPoints;
    A        : TValueAccuracyMinMax;
Begin
  // determine Y-height
  Y := 1.0;
  if assigned(FComparison) and (Length(FComparison.FInstruments) > 0) then     // draw instruments only if a comparison is loaded
    For NI := 0 to Length(FComparison.FInstruments)-1 do
      Y := Y + Length(FComparison.FInstruments[NI].FRanges[qtDCV])*1.0 + 1.0;  // +1 for instrument name and visual separation
  // setup diagram
  FCoord.FValYMax := Y;
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
                    if (TP.FValues[NP] <> 0.0) and (A.FMin > TBipolarSemiLogXBase(FDiagram.FCoord).FValXPosMin) then
                      FDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, FAccuracyPenColor, psSolid, Round(FAccuracyPenWidth), FAccuracyBrushColor, bsSolid);
                      // some min/max values are smaller than the smallest decade in the SemiLogY diagram
                    A.Free;
                    FDiagram.DrawSymPlus(TP.FValues[NP], Y, FTestPointLenDrw, FTestPointPenColor, psSolid, Round(FTestPointPenWidth));
                  End;
              End;
            FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel2Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
              FLabelFontName, Round(FLabelFontSize), FloatToStr(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FMaxValue));
            Y := Y + 1.0;
          End;
        FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
          FLabelFontName, Round(FLabelFontSize), FComparison.FInstruments[NI].FName+' '+CInstrumentFunction[FComparison.FInstruments[NI].FFunction]);
        Y := Y + 1.0;
      End;
End;

Procedure TComparisonDiagrams.DrawProcedure(AWidth, AHeight : Integer; ACanvas : TFPCustomCanvas);
Var NS,NR,NP : Integer;
    Y        : Double;
    St       : String;
Begin
  // determine Y-height
  Y := 1.0;
  if assigned(FComparison) and assigned(FComparison.FProcedure) then     // draw procedure only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Begin
        For NR := 0 to Length(FComparison.FProcedure.FSets[NS].FRanges)-1 do
          if assigned(FComparison.FProcedure.FSets[NS].FRanges[NR]) then
            Y := Y + 1.0;
        Y := Y + 1.0;
      End;
  // setup diagram
  FCoord.FValYMax := Y;
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
              FLabelFontName, Round(FLabelFontSize), St);
            Y := Y + 1.0;
          End;
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, FTestPointLenDrw, FTestPointPenColor, psSolid, Round(FTestPointPenWidth));
          End;
        St := 'Set #'+IntToStr(NS)+' to '+FloatToStr(FComparison.FProcedure.FSets[NS].FMaxVal);
        FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
          FLabelFontName, Round(FLabelFontSize), St);
        Y := Y + 1.0;
      End;
  if assigned(FComparison) and assigned(FComparison.FProcedure) then
    FComparison.FProcedure.PrintRanges;
End;

Procedure TComparisonDiagrams.DrawResults(AWidth, AHeight : Integer; ACanvas : TFPCustomCanvas);
Var NS,NI,NP  : Integer;
    Y         : Double;
    AllRanges : TInstrumentRanges;
    St        : String;
    TPMins,
    TPMaxs    : TDynDoubleArray;
    A         : TValueAccuracyMinMax;
Begin
  // determine Y-height
  Y := 1.0;
  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then     // draw results only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Y := Y + Length(FComparison.FInstruments)*1.0 + 1.0;  // +1 for instrument name and visual separation
  // setup diagram
  FCoord.FValYMax := Y;
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
        // compare measurement values
        SetLength(TPMins, Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues));  // determine highest lower limit of accuracy ranges, separately for each set
        SetLength(TPMaxs, Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues));  // determine lowest  upper limit of accuracy ranges, separately for each set
        For NP := 0 to Length(TPMins)-1 do  TPMins[NP] := -MaxDouble;
        For NP := 0 to Length(TPMaxs)-1 do  TPMaxs[NP] := +MaxDouble;
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
            Begin
              A := FComparison.FProcedure.FSets[NS].FMeasurements[NI][NP].FValue as TValueAccuracyMinMax;
              TPMins[NP] := max(TPMins[NP], A.FMin);
              TPMaxs[NP] := min(TPMaxs[NP], A.FMax);
            End;
        // per instrument: range bar and text
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          Begin
            FDiagram.DrawRange(AllRanges[NI].FResolution, AllRanges[NI].FMaxValue, Y, 0.8,
              FRangePenColor, psSolid, Round(FRangePenWidth), FRangeBrushColor, bsSolid);
            St := FComparison.FInstruments[NI].FName+' '+FloatToStr(AllRanges[NI].FMaxValue);
            FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel2Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0,
              FLabelFontName, Round(FLabelFontSize), St);
            Y := Y + 1.0;
          End;
        Y := Y - Length(FComparison.FInstruments)*1.0;
        // draw overlap region
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            if TPMins[NP] < TPMaxs[NP] then
              FDiagram.DrawRect(TPMins[NP], TPMaxs[NP], Y-0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
                FOvlpOkPenColor, psSolid, Round(FOvlpOkPenWidth), FOvlpOkBrushColor, bsSolid)
            else
              FDiagram.DrawRect(TPMaxs[NP], TPMins[NP], Y+0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
                FOvlpNOkPenColor, psSolid, Round(FOvlpNOkPenWidth), FOvlpNOkBrushColor, bsSolid);
          End;
        // draw measurement results
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          Begin
            // FMeasurements[InstrumentIdx][TestPointIdx]
            For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
              Begin
                A := FComparison.FProcedure.FSets[NS].FMeasurements[NI][NP].FValue as TValueAccuracyMinMax;
                //if (FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP] <> 0.0) {and (A.FMin > TBipolarSemiLogXBase(FResultsDiagram.FCoord).FValXPosMin)} then
                  FDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, FAccuracyPenColor, psSolid, Round(FAccuracyPenWidth), FAccuracyBrushColor, bsSolid);
                  // some min/max values are smaller than the smallest decade in the SemiLogY diagram
                FDiagram.DrawSymPlus(A.FValue, Y, FResultLenDrw, FResultPenColor, psSolid, Round(FResultPenWidth));
              End;
            Y := Y + 1.0;
          End;
        // draw testpoints in green as reference
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            FDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, FTestPointLenDrw, FTestPointRefPenColor, psSolid, Round(FTestPointPenWidth));
          End;
        St := 'Set #'+IntToStr(NS)+' to '+FloatToStr(FComparison.FProcedure.FSets[NS].FMaxVal);
        FDiagram.FVecPage.AddText(FDiagram.FDiagBox.Left+FLabel1Indent, FDiagram.FDiagBox.Bottom+FDiagram.FCoord.ValY2Drw(Y)-FLabelFontSize*0.5, 0.0, FLabelFontName, Round(FLabelFontSize), St);
        Y := Y + 1.0;
      End;
  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then
    Begin
      FComparison.FProcedure.PrintMeasurementsByInstrument;
      FComparison.FProcedure.PrintMeasurementsByTestPoint;
    End;
End;

End.

