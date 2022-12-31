Program GenReport;

{$mode objfpc}{$H+}

Uses  SysUtils, Classes, Variants,
      Instrument,
      Comparison, Diagrams, Report,
      FPImage, FPCanvas,
      Interfaces,
      VectorialDiagram, fpvectorial, svgvectorialwriter;

Procedure Usage;
Begin
  WriteLn(ParamStr(0),' [-o outfile] [-a author] [-d] filename');
  Halt(1);
End;

Var I            : Integer;
    FInFilename  : String;
    FOutFilename : String;
    FAuthor      : String;
    FDebug       : Boolean;
    FComparison  : TComparisonBase;
    FReport      : TComparisonReport;
    S            : TStringList;

Begin
  // parse parameters
  FInFilename  := '';
  FOutFilename := '';
  FAuthor      := '';
  FDebug       := False;
  I := 1;
  While I <= ParamCount do
    Begin
      if ParamStr(I) = '-o' then
        Begin
          Inc(I);
          FOutFilename := ParamStr(I);
        End
      else if ParamStr(I) = '-a' then
        Begin
          Inc(I);
          FAuthor := ParamStr(I);
        End
      else if ParamStr(I) = '-d' then
        FDebug := True
      else
        FInFilename := ParamStr(I);
      Inc(I);
    End;
  if FInFilename = '' then
    Usage;   // doesn't return

  // prepare factories
  SetupInstrumentWrapperFactory;
  SetupMeasurementResultFactory;
  SetupObjectFactory;

  try
    FComparison := TComparisonBase.Load(FInFilename);
  except
    on E : Exception do
      Begin
        WriteLn('Error loading '+FInFilename+': '+E.Message);
        Halt(1);
      End;
  End;
  // print
  if FDebug and assigned(FComparison) and assigned(FComparison.FProcedure) then
    Begin
      WriteLn('#### Measurement Procedure ####');
      FComparison.FProcedure.PrintRanges;
    End;
  if FDebug and assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then
    Begin
      WriteLn;
      WriteLn('#### Measurement Results by Instrument ####');
      FComparison.FProcedure.PrintMeasurementsByInstrument;
      WriteLn;
      WriteLn('#### Measurement Results by Testpoint ####');
      FComparison.FProcedure.PrintMeasurementsByTestPoint;
    End;

  // setup report
  FReport := TComparisonReport.Create(FInFilename, FComparison);
  if FOutFilename > '' then
    FReport.FOutFilename := FOutFilename;
  FReport.FAuthor := FAuthor;
  // generate report
  S := FReport.GenReport;
  // save report, all referenced files were already saved by GenReport
  S.SaveToFile(FReport.FOutFilename);
  S.Free;
  WriteLn('Saved report to ',FReport.FOutFilename,'. Compile with');
  WriteLn('  $ pdflatex --shell-escape ',FReport.FOutFilename);
End.

