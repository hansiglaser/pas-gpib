Unit MainForm;

{$mode objfpc}{$H+}
{ $ m odeswitch advancedrecords}

{ $ D EFINE CreateMeasureDefinitionFirstAttempt}

Interface

Uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Grids, Menus, Spin, FPCanvas, FPImage, Instrument, VectorialDiagram,
  Comparison, Diagrams;

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
    FRangesNeedsRedraw    : Boolean;
    FProcedureNeedsRedraw : Boolean;
    FResultsNeedsRedraw   : Boolean;
    // "business" data and procedures
    FFilename                 : String;
    FComparison               : TComparisonBase;
    FRangesDiag               : TComparisonDiagrams;
    FProcedureDiag            : TComparisonDiagrams;
    FResultsDiag              : TComparisonDiagrams;
    Procedure LoadFile;
    Procedure UpdateGui;
    Procedure SaveFile;
    Procedure WriteReport;
  End;

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
  // prepare diagrams
  FRangesDiag    := TComparisonDiagrams.Create(FComparison);
  FProcedureDiag := TComparisonDiagrams.Create(FComparison);
  FResultsDiag   := TComparisonDiagrams.Create(FComparison);
  // GUI control housekeeping
  FRangesNeedsRedraw    := True;
  FProcedureNeedsRedraw := True;
  FResultsNeedsRedraw   := True;
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
  if FRangesNeedsRedraw then
    Begin
      ImgRanges.Picture.Bitmap.SetSize(ImgRanges.Width, ImgRanges.Height);
      FRangesDiag.DrawRanges(ImgRanges.Width, ImgRanges.Height, ImgRanges.Canvas);
      FRangesNeedsRedraw := False;
    End;
  FRangesDiag.FDiagram.Paint(ImgRanges.Canvas);   // here we need the TImage.Canvas
  FRangesDiag.FDiagram.WriteSVG('test-ranges.svg');
End;

Procedure TFormMain.ImgRangesResize(Sender : TObject);
Begin
  FRangesNeedsRedraw := True;
End;

Procedure TFormMain.ImgProcedurePaint(Sender : TObject);
Begin
  if FProcedureNeedsRedraw then
    Begin
      ImgProcedure.Picture.Bitmap.SetSize(ImgProcedure.Width, ImgProcedure.Height);
      FProcedureDiag.DrawProcedure(ImgProcedure.Width, ImgProcedure.Height, ImgProcedure.Canvas);
      FProcedureNeedsRedraw := False;
    End;
  FProcedureDiag.FDiagram.Paint(ImgProcedure.Canvas);   // here we need the TImage.Canvas
  FProcedureDiag.FDiagram.WriteSVG('test-procedure.svg');
End;

Procedure TFormMain.ImgProcedureResize(Sender : TObject);
Begin
  FProcedureNeedsRedraw := True;
End;

Procedure TFormMain.ImgResultsPaint(Sender : TObject);
Begin
  if FResultsNeedsRedraw then
    Begin
      ImgResults.Picture.Bitmap.SetSize(ImgResults.Width, ImgResults.Height);
      FResultsDiag.DrawResults(ImgResults.Width, ImgResults.Height, ImgResults.Canvas);
      FResultsNeedsRedraw := False;;
    End;
  FResultsDiag.FDiagram.Paint(ImgResults.Canvas);   // here we need the TImage.Canvas
  FResultsDiag.FDiagram.WriteSVG('test-results.svg');
End;

Procedure TFormMain.ImgResultsResize(Sender : TObject);
Begin
  FResultsNeedsRedraw := True;
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
  FRangesDiag.FComparison := FComparison;
  FRangesDiag.DrawRanges(ImgRanges.Width, ImgRanges.Height, ImgRanges.Canvas);
  FRangesDiag.FDiagram.Paint(ImgRanges.Canvas);
  if Length(FComparison.FInstruments) > 0 then
    Begin
      BtnGenTestPoints.Enabled := True;
      if Length(FComparison.FInstruments[0].FTestpoints) > 0 then
        BtnGenProcedure.Enabled := True;
    End;
  // diagram for the measurement procedure
  FProcedureDiag.FComparison := FComparison;
  FProcedureDiag.DrawProcedure(ImgProcedure.Width, ImgProcedure.Height, ImgProcedure.Canvas);
  FProcedureDiag.FDiagram.Paint(ImgProcedure.Canvas);
  // information on procedure
  if assigned (FComparison.FProcedure) then
    LbNumTestpoints.Caption := 'Total '+IntToStr(FComparison.FProcedure.GetNumSets)+' sets with '+IntToStr(FComparison.FProcedure.GetNumTestpoints)+' testpoints'
  else
    LbNumTestpoints.Caption := 'No procedure defined';
  // diagram for the result
  FResultsDiag.FComparison := FComparison;
  FResultsDiag.DrawResults(ImgResults.Width, ImgResults.Height, ImgResults.Canvas);
  FResultsDiag.FDiagram.Paint(ImgResults.Canvas);
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
End;

End.

