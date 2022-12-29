Program GenReport;

{$mode objfpc}{$H+}

Uses  SysUtils, Classes, Variants,
      Instrument,
      Comparison, Diagrams, Report,
      FPImage, FPCanvas,
      Interfaces,
      VectorialDiagram, fpvectorial, svgvectorialwriter,
      uTEngine, uTTemplate, uTContext, uTContextManager;

procedure TestTemplate;
var
	definitionFile, templateFile, dirResultFile, extResultFile: String;
  definition, fields: TStringList;
  i, j: integer;
  className: String;
  fieldsArray: array of Variant;
	template: TTemplate;
  context: IContext;
  engine: TEngine;
begin
  definitionFile := 'delphi-templeng-code/Example1_ClassCodeGenerator/Input/Person.txt';
  if (Length(definitionFile) <= 0) then
  	raise Exception.Create('Informe um arquivo de entrada.');
  if (not FileExists(definitionFile)) then
  	raise Exception.Create('Informe de entrada não existe.');

	case (1{CBLanguage.ItemIndex}) of
  	0: begin
    	templateFile := 'delphi-templeng-code/Example1_ClassCodeGenerator/Templates/Java.tpt';
      extResultFile := 'java';
    end;
    1: begin
    	templateFile := 'delphi-templeng-code/Example1_ClassCodeGenerator/Templates/Delphi.tpt';
      extResultFile := 'pas';
    end;
  	else raise Exception.Create('Selecione uma linguagem.');
	end;

//  if (Length(EdOutput.Text) <= 0) then
//    raise Exception.Create('Informe um diretório de saída.');
  dirResultFile := 'delphi-templeng-code/Example1_ClassCodeGenerator/Output/';

  engine := nil;
  template := nil;
  definition := nil;
  fields := nil;
  try
    engine := TEngine.Create;
    template := TTemplate.Create;
  	definition := TStringList.Create;
  	fields := TStringList.Create;

    template.loadTemplateFile(templateFile); // Load the template

  	definition.LoadFromFile(definitionFile); // Load the definition file

    for i := 0 to definition.Count - 1 do // For each class in the definiion file
    begin
      className := definition.Names[i]; // Get the name of the class
      if (Length(className) > 0) then
      begin
        fields.CommaText := definition.ValueFromIndex[i]; // Get the fields of the class

        // Build the array of class fields
        SetLength(fieldsArray, fields.Count);
        for j := 0 to fields.Count - 1 do
          fieldsArray[j] := fields[j];

        context := TContextManager.getInstance.getNewContext; // Create the context
        context.put('class_name', className); // Set the class name to the context
        context.put('fields', fieldsArray); // Set the class fields to the context

        engine.merge(template, context); // Process the file

        engine.saveResultTextToFile(dirResultFile + className + '.' + extResultFile); // Save the output
      end;
    end;
  finally
  	fields.Free;
  	definition.Free;
    template.Free;
    engine.Free;
  end;

  WriteLn('Documentos gerados com sucesso.');
end;

Var FFilename   : String;
    FComparison : TComparisonBase;
    FReport     : TComparisonReport;

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

  FReport := TComparisonReport.Create(FComparison);
  FReport.WriteReport('testreport2.tex');

  TestTemplate;
End.

