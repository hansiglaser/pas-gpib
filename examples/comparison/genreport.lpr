Program GenReport;

{$mode objfpc}{$H+}

Uses  SysUtils, Classes, Variants,
      Instrument,
      Comparison, Diagrams, Report,
      FPImage, FPCanvas,
      Interfaces,
      VectorialDiagram, fpvectorial, svgvectorialwriter;

Var FFilename   : String;
    FComparison : TComparisonBase;
    FReport     : TComparisonReport;
    S           : TStringList;

Begin
  // prepare factories
  SetupInstrumentWrapperFactory;
  SetupMeasurementResultFactory;
  SetupObjectFactory;

  FFilename   := 'E36313A_Ch2s3-U1253B-DCV-out.InstCmp';
  FComparison := TComparisonBase.Load(FFilename);
  FComparison.SetupRanges;
  FComparison.FQuantity := qtDCV;
  if assigned(FComparison) and assigned(FComparison.FProcedure) and (Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0) then
    Begin
      FComparison.FProcedure.PrintMeasurementsByInstrument;
      FComparison.FProcedure.PrintMeasurementsByTestPoint;
    End;

  FReport := TComparisonReport.Create(FFilename, FComparison);
  FReport.FAuthor := 'Me & Myself';
  S := FReport.GenReport;
  // save
  S.SaveToFile(FReport.FOutFilename);
  S.Free;
End.

