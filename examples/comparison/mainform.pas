Unit MainForm;

{$mode objfpc}{$H+}
{ $ m odeswitch advancedrecords}

{ $ D EFINE CreateMeasureDefinitionFirstAttempt}

Interface

Uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Grids, Menus, Spin, FPCanvas, FPImage, Instrument, VectorialDiagram,
  Comparison;

Type

  { TFormMain }

  TFormMain = Class(TForm)
    // GUI controls and event handlers
    StatusBar1 : TStatusBar;
    ToolBar1 : TToolBar;
    BtnOpen : TToolButton;
    BtnReload : TToolButton;
    BtnSave : TToolButton;
    BtnSaveAs : TToolButton;
    BtnSep1 : TToolButton;
    BtnSep2 : TToolButton;
    BtnQuit: TToolButton;
    ILToolBar : TImageList;
    PageControl1 : TPageControl;
    TabSheet1 : TTabSheet;
    SGInstruments : TStringGrid;
    TabSheet2 : TTabSheet;
    ImgRanges : TImage;
    BtnGenTestPoints : TButton;
    SENumLinPoints : TSpinEdit;
    LbNumLinPoints : TLabel;
    BtnGenProcedure : TButton;
    TabSheet3 : TTabSheet;
    ImgProcedure : TImage;
    TabSheet4 : TTabSheet;
    ImgResults : TImage;
    CBQuantity : TComboBox;
    LbQuantity : TLabel;
    BtnReport : TToolButton;
    OpenDialog : TOpenDialog;
    SaveDialog : TSaveDialog;
    LbNumTestpoints : TLabel;
    Procedure FormCreate(Sender : TObject);
    Procedure FormDestroy(Sender : TObject);
    Procedure BtnOpenClick(Sender : TObject);
    Procedure BtnReloadClick(Sender : TObject);
    Procedure BtnSaveClick(Sender : TObject);
    Procedure BtnSaveAsClick(Sender : TObject);
    Procedure BtnReportClick(Sender : TObject);
    Procedure BtnQuitClick(Sender : TObject);
    Procedure BtnGenTestPointsClick(Sender : TObject);
    Procedure BtnGenProcedureClick(Sender : TObject);
    Procedure ImgRangesPaint(Sender : TObject);
    Procedure ImgRangesResize(Sender : TObject);
    Procedure ImgProcedurePaint(Sender : TObject);
    Procedure ImgProcedureResize(Sender : TObject);
    Procedure ImgResultsPaint(Sender : TObject);
    Procedure ImgResultsResize(Sender : TObject);
    Procedure StatusBar1Resize(Sender : TObject);
  public
    // GUI controls housekeeping
    FRangesWasResized    : Boolean;
    FProcedureWasResized : Boolean;
    FResultsWasResized   : Boolean;
    // "business" data and procedures
    FFilename                 : String;
    FComparison               : TComparisonBase;
    FRangesCoord              : TBipolarSemiLogXBase;
    FRangesDiagram            : TVectorialDiagram;
    FProcedureCoord           : TBipolarSemiLogXBase;
    FProcedureDiagram         : TVectorialDiagram;
    FResultsCoord             : TBipolarSemiLogXBase;
    FResultsDiagram           : TVectorialDiagram;
    Procedure LoadFile;
    Procedure UpdateGui;
    Procedure SaveFile;
    Procedure WriteReport;
    Procedure DrawRanges;
    Procedure DrawProcedure;
    Procedure DrawResults;
  End;

Const
  colLtRed      : TFPColor = (Red: $FFFF; Green: $C000; Blue: $C000; Alpha: alphaOpaque);
  colLtGreen    : TFPColor = (Red: $C000; Green: $FFFF; Blue: $C000; Alpha: alphaOpaque);   // this overwrites and hides the definition in FPImage
  colLtBlue     : TFPColor = (Red: $C000; Green: $C000; Blue: $FFFF; Alpha: alphaOpaque);
  colLtOrange   : TFPColor = (Red: $FFFF; Green: $E000; Blue: $C000; Alpha: alphaOpaque);

Var
  FormMain : TFormMain;

Implementation

{$R *.lfm}

{ TFormMain }

Procedure TFormMain.FormCreate(Sender : TObject);
Begin
  // prepare factories
  SetupInstrumentWrapperFactory;
  SetupMeasurementResultFactory;
  SetupObjectFactory;
  // prepare diagram for the ranges and testpoints
  FRangesCoord := TBipolarSemiLogXBase.Create;
  FRangesCoord.FValXNegMax   := 1E1 * 2.0;
  FRangesCoord.FValXNegMin   := 1E-1 * 0.5;
  //FRangesCoord.FValXNegMax   := 0.0;
  //FRangesCoord.FValXNegMin   := 0.0;
  FRangesCoord.FValXPosMin   := 1E-8 * 0.5;
  FRangesCoord.FValXPosMax   := 1E3 * 2.0;
  FRangesCoord.FValXWithZero := True;
  FRangesCoord.FValYMin      := 0.0;
  FRangesCoord.FValYMax      := 47.0;
  FRangesCoord.FDrwZeroWidth := 20.0;
  FRangesDiagram := TVectorialDiagram.Create(FRangesCoord, FormMain.ImgRanges);
  // prepare diagram for the measurement procedure
  FProcedureCoord := TBipolarSemiLogXBase.Create;
  FProcedureCoord.FValXNegMax   := 1E1 * 2.0;
  FProcedureCoord.FValXNegMin   := 1E-1 * 0.5;
  //FProcedureCoord.FValXNegMax   := 0.0;
  //FProcedureCoord.FValXNegMin   := 0.0;
  FProcedureCoord.FValXPosMin   := 1E-8 * 0.5;
  FProcedureCoord.FValXPosMax   := 1E3 * 2.0;
  FProcedureCoord.FValXWithZero := True;
  FProcedureCoord.FValYMin      := 0.0;
  FProcedureCoord.FValYMax      := 47.0;
  FProcedureCoord.FDrwZeroWidth := 20.0;
  FProcedureDiagram := TVectorialDiagram.Create(FProcedureCoord, FormMain.ImgProcedure);
  // prepare diagram for the result
  FResultsCoord := TBipolarSemiLogXBase.Create;
  FResultsCoord.FValXNegMax   := 1E1 * 2.0;
  FResultsCoord.FValXNegMin   := 1E-1 * 0.5;
  //FResultsCoord.FValXNegMax   := 0.0;
  //FResultsCoord.FValXNegMin   := 0.0;
  FResultsCoord.FValXPosMin   := 1E-8 * 0.5;
  FResultsCoord.FValXPosMax   := 1E3 * 2.0;
  FResultsCoord.FValXWithZero := True;
  FResultsCoord.FValYMin      := 0.0;
  FResultsCoord.FValYMax      := 47.0;
  FResultsCoord.FDrwZeroWidth := 20.0;
  FResultsDiagram := TVectorialDiagram.Create(FResultsCoord, FormMain.ImgResults);
  // GUI control housekeeping
  FRangesWasResized    := True;
  FProcedureWasResized := True;
  FResultsWasResized   := True;
  // set GUI defaults
  PageControl1.TabIndex := 0;
  UpdateGui;
End;

Procedure TFormMain.FormDestroy(Sender : TObject);
Begin

End;

Procedure TFormMain.BtnOpenClick(Sender : TObject);
Begin
  WriteLn('TODO: check if loaded data was modified, ask if save, ignore, or cancel');
  if not OpenDialog.Execute then
    Exit;    // user didn't click OK
  FreeAndNil(FComparison);
  FFilename := OpenDialog.FileName; // 'test-DCV.InstCmp';
  LoadFile;
  UpdateGui;
  BtnReload.Enabled := True;
  BtnSave.Enabled   := True;
  BtnSaveAs.Enabled := True;
  BtnReport.Enabled := True;
End;

Procedure TFormMain.BtnReloadClick(Sender : TObject);
Begin
  if FFilename = '' then
    raise Exception.Create('Can''t reload without a file previously loaded.');
  WriteLn('TODO: check if loaded data was modified, ask if save, ignore, or cancel');
  FreeAndNil(FComparison);
  LoadFile;
  UpdateGui;
End;

Procedure TFormMain.BtnSaveClick(Sender : TObject);
Begin
  if not assigned(FComparison) then
    raise Exception.Create('Can''t save without a comparison object.');
  if FFilename = '' then
    Begin
      if not SaveDialog.Execute then      // also asks if an existing file should be overwritten
        Exit;   // user didn't click OK
      FFilename := SaveDialog.FileName;
    End;
  SaveFile;
End;

Procedure TFormMain.BtnSaveAsClick(Sender : TObject);
Begin
  if not assigned(FComparison) then
    raise Exception.Create('Can''t save without a comparison object.');
  if FFilename > '' then
    SaveDialog.FileName := FFilename;
  if not SaveDialog.Execute then      // also asks if an existing file should be overwritten
    Exit;   // user didn't click OK
  FFilename := SaveDialog.FileName;
  SaveFile;
End;

Procedure TFormMain.BtnReportClick(Sender : TObject);
Begin
  if not assigned(FComparison) then
    raise Exception.Create('Can''t create a report without a comparison object.');
  WriteReport;
End;

Procedure TFormMain.BtnQuitClick(Sender : TObject);
Begin
  WriteLn('TODO: check if loaded data was modified, ask if save, ignore, or cancel');
  Close;
End;

Procedure TFormMain.BtnGenTestPointsClick(Sender : TObject);
Begin
  if not assigned(FComparison) then
    raise Exception.Create('Can''t create testpoints without a comparison object.');
  if Length(FComparison.FInstruments) = 0 then
    raise Exception.Create('Can''t create testpoints without instruments.');
  FComparison.CreateTestPoints(SENumLinPoints.Value);
  FComparison.PrintTestPoints;
  UpdateGui;
End;

Procedure TFormMain.BtnGenProcedureClick(Sender : TObject);
Begin
  if not assigned(FComparison) then
    raise Exception.Create('Can''t generate measurement procedure without a comparison object.');
  if Length(FComparison.FInstruments) = 0 then
    raise Exception.Create('Can''t generate measurement procedure without instruments.');
  if Length(FComparison.FInstruments[0].FTestpoints) = 0 then
    raise Exception.Create('Can''t generate measurement procedure without testpoins.');
{$IFDEF CreateMeasureDefinitionFirstAttempt}
  FComparison.CreateMeasureDefinitionFirstAttempt;
{$ENDIF} // CreateMeasureDefinitionFirstAttempt
  FComparison.CreateMeasureDefinition;
  UpdateGui;
  PageControl1.TabIndex := 2;
End;

Procedure TFormMain.ImgRangesPaint(Sender : TObject);
Begin
  if FRangesWasResized then
    Begin
      ImgRanges.Picture.Bitmap.SetSize(ImgRanges.Width, ImgRanges.Height);
      DrawRanges;
      FRangesWasResized := False;
    End;
  FRangesDiagram.Update;
End;

Procedure TFormMain.ImgRangesResize(Sender : TObject);
Begin
  FRangesWasResized := True;
End;

Procedure TFormMain.ImgProcedurePaint(Sender : TObject);
Begin
  if FProcedureWasResized then
    Begin
      ImgProcedure.Picture.Bitmap.SetSize(ImgProcedure.Width, ImgProcedure.Height);
      DrawProcedure;
      FProcedureWasResized := False;
    End;
  FProcedureDiagram.Update;
End;

Procedure TFormMain.ImgProcedureResize(Sender : TObject);
Begin
  FProcedureWasResized := True;
End;

Procedure TFormMain.ImgResultsPaint(Sender : TObject);
Begin
  if FResultsWasResized then
    Begin
      ImgResults.Picture.Bitmap.SetSize(ImgResults.Width, ImgResults.Height);
      DrawResults;
      FResultsWasResized := False;;
    End;
  FResultsDiagram.Update;
End;

Procedure TFormMain.ImgResultsResize(Sender : TObject);
Begin
  FResultsWasResized := True;
End;

Procedure TFormMain.StatusBar1Resize(Sender : TObject);
Var H : TFPCHeapStatus;
Begin
  H := GetFPCHeapStatus;
  StatusBar1.Panels[1].Text := 'Memory: '+IntToStr(H.CurrHeapUsed);
End;

Procedure TFormMain.LoadFile;
Begin
  if assigned(FComparison) then
    raise Exception.Create('Can''t create a comparison with one still existing. Did you forget to Free?');

{$IFDEF LoadDefaultInstruments}
  FComparison := TComparisonBase.Create;
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'Fluke177',         'Fluke177_0',    TInstrumentWrapperParams.Create));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'KeysightU1253B',   'U1253B_0',      TInstrumentWrapperParams.Create));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'Agilent34461A',    '34461A_0',      TInstrumentWrapperParams.Create));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'KeithleyDMM6500',  'DMM6500_0',     TInstrumentWrapperParams.Create(['TestInteger',TParamValueInteger.Create(123),'TestString',TParamValueString.Create('MyString'),'TestFunction', TParamValueFunction.Create(ifMeasure)])));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'Keithley2450',     '2450_0M',       TInstrumentWrapperParams.Create('Function',TParamValueFunction.Create(ifMeasure))));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'Keithley2450',     '2450_0S',       TInstrumentWrapperParams.Create('Function',TParamValueFunction.Create(ifSource ))));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'KeysightE36313A',  'E36313A_Ch23M', TInstrumentWrapperParams.Create('Function',TParamValueFunction.Create(ifMeasure))));
  FComparison.AddInstrument(FInstrumentWrapperFactory.CreateWrapper(FComparison, 'KeysightE36313A',  'E36313A_Ch23S', TInstrumentWrapperParams.Create('Function',TParamValueFunction.Create(ifSource ))));*)
{$ELSE}
  try
    FComparison := TComparisonBase.Load(FFilename);
  except
    on E : Exception do
      Begin
        FComparison := Nil;
        raise;
      End;
  End;
{$ENDIF}   // LoadDefaultInstruments
  FComparison.SetupRanges;
  FComparison.FQuantity := qtDCV;
End;

Procedure TFormMain.UpdateGui;
Var I : Integer;
Begin
  WriteLn('UpdateGui');
  // toolbar
  if not assigned(FComparison) then
    Begin
      BtnReload.Enabled        := False;
      BtnSave.Enabled          := False;
      BtnSaveAs.Enabled        := False;
      BtnReport.Enabled        := False;
      BtnGenTestPoints.Enabled := False;  // TODO: depends
      BtnGenProcedure.Enabled  := False;  // TODO: depends
      Exit;
    End;
  // status bar
  StatusBar1.Panels.Items[0].Text := FFilename;
  // table with instruments
  SGInstruments.RowCount := Length(FComparison.FInstruments)+1;   // +1 because the title row also counts
  For I := 0 to Length(FComparison.FInstruments)-1 do
    Begin
      SGInstruments.Cells[0,I+1] := FComparison.FInstruments[I].FName;
      SGInstruments.Cells[1,I+1] := FComparison.FInstruments[I].FWrapperName;
      SGInstruments.Cells[2,I+1] := FComparison.FInstruments[I].FParams.ToSyntax;
    End;
  // drop-down for quantity
  CBQuantity.ItemIndex := Integer(FComparison.FQuantity);   // be sure that CBQuantity.Items is aligned with TQuantity
  // diagram for ranges and testpoints
  DrawRanges;
  FRangesDiagram.Update;
  if Length(FComparison.FInstruments) > 0 then
    Begin
      BtnGenTestPoints.Enabled := True;
      if Length(FComparison.FInstruments[0].FTestpoints) > 0 then
        BtnGenProcedure.Enabled := True;
    End;
  // diagram for the measurement procedure
  DrawProcedure;
  FProcedureDiagram.Update;
  // information on procedure
  if assigned (FComparison.FProcedure) then
    LbNumTestpoints.Caption := 'Total '+IntToStr(FComparison.FProcedure.GetNumSets)+' sets with '+IntToStr(FComparison.FProcedure.GetNumTestpoints)+' testpoints'
  else
    LbNumTestpoints.Caption := 'No procedure defined';
  // diagram for the result
  DrawResults;
  FResultsDiagram.Update;
  WriteLn('TODO: UpdateGui further');
End;

Procedure TFormMain.SaveFile;
Begin
  if not assigned(FComparison) then
    raise Exception.Create('Can''t save a comparison without an instance of it.');
  FComparison.Save(FFilename);
End;

Procedure TFormMain.WriteReport;
Begin
  WriteLn('TODO: Create report');
  // see also TVectorialDiagram.Update which formerly wrote an SVG file
End;

Procedure TFormMain.DrawRanges;
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
  FRangesCoord.FValYMax := Y;
  FRangesDiagram.Resize;
  FRangesDiagram.DrawAxes;
  // draw ranges and testpoints
  Y := 1.0;
  if assigned(FComparison) and (Length(FComparison.FInstruments) > 0) then     // draw instruments only if a comparison is loaded
    For NI := 0 to Length(FComparison.FInstruments)-1 do
      Begin
        For NR := 0 to Length(FComparison.FInstruments[NI].FRanges[qtDCV])-1 do
          Begin
            FRangesDiagram.DrawRange(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FResolution, FComparison.FInstruments[NI].FRanges[qtDCV][NR].FMaxValue, Y, 0.8,
              colBlack, psSolid, 1, colLtOrange, bsSolid);
            if Length(FComparison.FInstruments[NI].FTestpoints) > 0 then
              Begin
                // draw testpoints only if if they were generated
                TP := FComparison.FInstruments[NI].FTestpoints[NR];
                For NP := 0 to Length(TP.FValues)-1 do
                  Begin
                    A := TValueAccuracyMinMax(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FAccuracy[0].Apply(TP.FValues[NP]));
                    if (TP.FValues[NP] <> 0.0) and (A.FMin > TBipolarSemiLogXBase(FRangesDiagram.FCoord).FValXPosMin) then
                      FRangesDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, colBlack, psSolid, 1, colLtBlue, bsSolid);
                      // some min/max values are smaller than the smallest decade in the SemiLogY diagram
                    A.Free;
                    FRangesDiagram.DrawSymPlus(TP.FValues[NP], Y, 4.0, colRed, psSolid, 1);
                  End;
              End;
            FRangesDiagram.FVecPage.AddText(FRangesDiagram.FDiagBox.Left*1.8, FRangesDiagram.FDiagBox.Bottom+FRangesDiagram.FCoord.ValY2Drw(Y)-5, 0.0, 'Arial', 10, FloatToStr(FComparison.FInstruments[NI].FRanges[qtDCV][NR].FMaxValue));
            Y := Y + 1.0;
          End;
        FRangesDiagram.FVecPage.AddText(FRangesDiagram.FDiagBox.Left*1.2, FRangesDiagram.FDiagBox.Bottom+FRangesDiagram.FCoord.ValY2Drw(Y)-5, 0.0, 'Arial', 10, FComparison.FInstruments[NI].FName+' '+CInstrumentFunction[FComparison.FInstruments[NI].FFunction]);
        Y := Y + 1.0;
      End;
End;

Procedure TFormMain.DrawProcedure;
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
  FProcedureCoord.FValYMax := Y;
  FProcedureDiagram.Resize;
  FProcedureDiagram.DrawAxes;
  // draw comparison procedure
  Y := 1.0;
  if assigned(FComparison) and assigned(FComparison.FProcedure) then     // draw procedure only if it exists
    For NS := 0 to Length(FComparison.FProcedure.FSets)-1  do
      Begin
        For NR := 0 to Length(FComparison.FProcedure.FSets[NS].FRanges)-1 do
          Begin
            if not assigned(FComparison.FProcedure.FSets[NS].FRanges[NR]) then continue;
            FProcedureDiagram.DrawRange(FComparison.FProcedure.FSets[NS].FRanges[NR].FResolution, FComparison.FProcedure.FSets[NS].FRanges[NR].FMaxValue, Y, 0.8,
              colBlack, psSolid, 1, colLtOrange, bsSolid);
            St := FComparison.FProcedure.FSets[NS].FComparison.FInstruments[NR].FName+' '+FloatToStr(FComparison.FProcedure.FSets[NS].FRanges[NR].FMaxValue);
            FProcedureDiagram.FVecPage.AddText(FProcedureDiagram.FDiagBox.Left*1.8, FProcedureDiagram.FDiagBox.Bottom+FProcedureDiagram.FCoord.ValY2Drw(Y)-5, 0.0, 'Arial', 10, St);
            Y := Y + 1.0;
          End;
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            FProcedureDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, 4.0, colRed, psSolid, 1);
          End;
        St := 'Set #'+IntToStr(NS)+' to '+FloatToStr(FComparison.FProcedure.FSets[NS].FMaxVal);
        FProcedureDiagram.FVecPage.AddText(FProcedureDiagram.FDiagBox.Left*1.2, FProcedureDiagram.FDiagBox.Bottom+FProcedureDiagram.FCoord.ValY2Drw(Y)-5, 0.0, 'Arial', 10, St);
        Y := Y + 1.0;
      End;
  if assigned(FComparison) and assigned(FComparison.FProcedure) then
    FComparison.FProcedure.PrintRanges;
End;

Procedure TFormMain.DrawResults;
Var NS,NI,NP  : Integer;
    Y         : Double;
    AllRanges : TInstrumentRanges;
    Prev      : TComparisonSet;
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
  FResultsCoord.FValYMax := Y;
  FResultsDiagram.Resize;
  FResultsDiagram.DrawAxes;
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
            FResultsDiagram.DrawRange(AllRanges[NI].FResolution, AllRanges[NI].FMaxValue, Y, 0.8,
              colBlack, psSolid, 1, colLtOrange, bsSolid);
            St := FComparison.FInstruments[NI].FName+' '+FloatToStr(AllRanges[NI].FMaxValue);
            FResultsDiagram.FVecPage.AddText(FResultsDiagram.FDiagBox.Left*1.8, FResultsDiagram.FDiagBox.Bottom+FResultsDiagram.FCoord.ValY2Drw(Y)-5, 0.0, 'Arial', 10, St);
            Y := Y + 1.0;
          End;
        Y := Y - Length(FComparison.FInstruments)*1.0;
        // draw overlap region
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            if TPMins[NP] < TPMaxs[NP] then
              FResultsDiagram.DrawRect(TPMins[NP], TPMaxs[NP], Y-0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
                colGreen, psSolid, 1, colLtGreen, bsSolid)
            else
              FResultsDiagram.DrawRect(TPMaxs[NP], TPMins[NP], Y+0.35, Y + Length(FComparison.FInstruments)*1.0 - 0.65,
                colRed, psSolid, 1, colLtRed, bsSolid);
          End;
        // draw measurement results
        For NI := 0 to Length(FComparison.FInstruments)-1 do
          Begin
            // FMeasurements[InstrumentIdx][TestPointIdx]
            For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
              Begin
                A := FComparison.FProcedure.FSets[NS].FMeasurements[NI][NP].FValue as TValueAccuracyMinMax;
                //if (FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP] <> 0.0) {and (A.FMin > TBipolarSemiLogXBase(FResultsDiagram.FCoord).FValXPosMin)} then
                  FResultsDiagram.DrawRange(A.FMin,A.FMax,Y, 0.5, colBlack, psSolid, 1, colLtBlue, bsSolid);
                  // some min/max values are smaller than the smallest decade in the SemiLogY diagram
                FResultsDiagram.DrawSymPlus(A.FValue, Y, 4.0, colBlack, psSolid, 1);
              End;
            Y := Y + 1.0;
          End;
        // draw testpoints in green as reference
        For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
          Begin
            FResultsDiagram.DrawSymPlus(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP], Y, 4.0, colDkGreen, psSolid, 1);
          End;
        St := 'Set #'+IntToStr(NS)+' to '+FloatToStr(FComparison.FProcedure.FSets[NS].FMaxVal);
        FResultsDiagram.FVecPage.AddText(FResultsDiagram.FDiagBox.Left*1.2, FResultsDiagram.FDiagBox.Bottom+FResultsDiagram.FCoord.ValY2Drw(Y)-5, 0.0, 'Arial', 10, St);
        Y := Y + 1.0;
      End;
  WriteLn('TODO: Draw results');
  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then
    Begin
      FComparison.FProcedure.PrintMeasurementsByInstrument;
      FComparison.FProcedure.PrintMeasurementsByTestPoint;
    End;
End;

End.

