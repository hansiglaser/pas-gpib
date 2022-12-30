Program Measurement;

{$mode ObjFPC}{$H+}

Uses Classes, SysUtils, Comparison, Instrument, PasGpibUtils;

Type

  { TComparisonMeasurement }

  TComparisonMeasurement = class
  private
    FInFilename     : String;
    FOutFilename    : String;
    FDisabled       : Array of String;
    FDisabledIdx    : Array of Boolean;
    FComparison     : TComparisonBase;
    FSourceIdx      : Integer;   // index into FComparison.FInstruments
    FSource         : TInstrumentWrapperBase;
    Procedure ParseParams;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Load;
    Procedure Save;
    Function  CheckComparison : Integer;   // on error, writes messages and returns the number of errors
    Procedure Initialize;
    Procedure Run;
    Procedure Disconnect;
  End;

{ TComparisonMeasurement }

Constructor TComparisonMeasurement.Create;
Begin
  inherited Create;
  ParseParams;
End;

Destructor TComparisonMeasurement.Destroy;
Begin
  FComparison.Free;
  Inherited Destroy;
End;

Procedure TComparisonMeasurement.Load;
Begin
  FComparison := TComparisonBase.Load(FInFilename);
  if not assigned(FComparison) then
    raise Exception.Create('FComparison was not created');
End;

Procedure TComparisonMeasurement.Save;
Begin
  FComparison.Save(FOutFilename);
End;

Function TComparisonMeasurement.CheckComparison : Integer;
Var I,J : Integer;
Begin
  Result := 0;
  // check if the disabled instruments are there
  SetLength(FDisabledIdx, Length(FComparison.FInstruments));
  For I := 0 to Length(FComparison.FInstruments)-1 do
    FDisabledIdx[I] := False;
  For I := 0 to Length(FDisabled)-1 do
    Begin
      J := FComparison.GetInstrumentIndex(FDisabled[I]);
      if J < 0 then
        Begin
          WriteLn('Error: Disabled instrument ',FDisabled[I],' is not defined.');
          Inc(Result);
          Continue;
        End;
      FDisabledIdx[J] := True;
    End;
  // check if we have at least 2 instruments
  J := 0;
  For I := 0 to Length(FComparison.FInstruments)-1 do
    if not FDisabledIdx[I] then
      Inc(J);
  if J < 2 then
    Begin
      WriteLn('Error: At least 2 (enabled) instruments required.');
      Inc(Result);
      Exit;
    End;
  // check that exactly 1 instrument is a source
  FSource := Nil;
  for I := 0 to Length(FComparison.FInstruments)-1 do
    if not FDisabledIdx[I] then
      if FComparison.FInstruments[I].FFunction = ifSource then
        Begin
          if assigned(FSource) then
            Begin
              WriteLn('Error: Multiple sources defined (',FSource.FName,', ',FComparison.FInstruments[I].FName,')');
              Inc(Result)
            End
          else
            Begin
              FSourceIdx := I;
              FSource    := FComparison.FInstruments[I];
            End;
        End;
  if FSource = Nil then
    Begin
      WriteLn('Error: No source defined');
      Inc(Result)
    End;
  // check if the comparison already has measurement results
  if Length(FComparison.FProcedure.FSets[0].FMeasurements) > 0 then
    Begin
      WriteLn('Error: The dataset already contains measurement results');
      Inc(Result);
    End;
End;

Procedure TComparisonMeasurement.Initialize;
Var NI : Integer;
Begin
  For NI := 0 to Length(FComparison.FInstruments)-1 do
    if not FDisabledIdx[NI] then
      FComparison.FInstruments[NI].Initialize;
End;

Procedure TComparisonMeasurement.Run;
Var NS,NI,NP : Integer;
Begin
  For NS := 0 to Length(FComparison.FProcedure.FSets)-1 do
    Begin
      WriteLn('### ');
      // set measurement ranges
      For NI := 0 to Length(FComparison.FInstruments)-1 do
        if not FDisabledIdx[NI] then
          if assigned(FComparison.FProcedure.FSets[NS].FRanges[NI]) then
            if FComparison.FInstruments[NI].FFunction = ifMeasure then
              FComparison.FInstruments[NI].SetMeasureRange(FComparison.FProcedure.FSets[NS].FRanges[NI]);
      // set source range
      if assigned(FComparison.FProcedure.FSets[NS].FRanges[FSourceIdx]) then
        FSource.SetSourceRange(FComparison.FProcedure.FSets[NS].FRanges[FSourceIdx]);
      // enable source output
      FSource.EnableOutput;
      // prepare storage of results
      SetLength(FComparison.FProcedure.FSets[NS].FMeasurements, Length(FComparison.FInstruments));
      For NI := 0 to Length(FComparison.FInstruments)-1 do
        if not FDisabledIdx[NI] then
          SetLength(FComparison.FProcedure.FSets[NS].FMeasurements[NI], Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues));
      // set and measure testpoints
      For NP := 0 to Length(FComparison.FProcedure.FSets[NS].FTestPoints.FValues)-1 do
        Begin
          FSource.SetSource(FComparison.FProcedure.FSets[NS].FTestPoints.FValues[NP]);
          For NI := 0 to Length(FComparison.FInstruments)-1 do
            if not FDisabledIdx[NI] then
              if FComparison.FInstruments[NI].FFunction = ifMeasure then
                FComparison.FInstruments[NI].StartMeasurement;
          For NI := 0 to Length(FComparison.FInstruments)-1 do
            if not FDisabledIdx[NI] then
              if FComparison.FInstruments[NI].FFunction = ifMeasure then
                FComparison.FProcedure.FSets[NS].FMeasurements[NI][NP] := FComparison.FInstruments[NI].GetResults
              else
                FComparison.FProcedure.FSets[NS].FMeasurements[NI][NP] := FComparison.FInstruments[NI].GetSourceValue;
        End;
      // disable source output
      FSource.DisableOutput;
    End;
End;

Procedure TComparisonMeasurement.Disconnect;
Var NI : Integer;
Begin
  For NI := 0 to Length(FComparison.FInstruments)-1 do
    if not FDisabledIdx[NI] then
      FComparison.FInstruments[NI].Disconnect;
End;

Procedure TComparisonMeasurement.ParseParams;
Var I : Integer;
Begin
  if ParamCount = 0 then
    Begin
      WriteLn('Usage: ',ParamStr(0),' -i infile [-o outfile] [-d instrument]');
      Halt;
    End;
  I := 1;
  While I <= ParamCount do
    Begin
      if ParamStr(I) = '-i' then
        Begin
          Inc(I);
          if I > ParamCount then
            Begin
              WriteLn('Error: -i needs a value');
              Halt(1);
            End;
          if FInFilename > '' then
            Begin
              WriteLn('Error: -i can only be specified once');
              Halt(1);
            End;
          FInFilename := ParamStr(I);
          if FOutFilename = '' then FOutFilename := FInFilename;
        End
      else if ParamStr(I) = '-o' then
        Begin
          Inc(I);
          if I > ParamCount then
            Begin
              WriteLn('Error: -o needs a value');
              Halt(1);
            End;
          if (FOutFilename > '') and (FInFilename <> FOutFilename) then
            Begin
              WriteLn('Error: -o can only be specified once');
              Halt(1);
            End;
          FOutFilename := ParamStr(I);
        End
      else if ParamStr(I) = '-d' then
        Begin
          Inc(I);
          if I > ParamCount then
            Begin
              WriteLn('Error: -o needs a value');
              Halt(1);
            End;
          SetLength(FDisabled, Length(FDisabled)+1);
          FDisabled[Length(FDisabled)-1] := ParamStr(I);
        End
      else
        Begin
          WriteLn('Error: Invalid parameter ',ParamStr(I));
          Halt(1);
        End;
      Inc(I);
    End;
  I := 0;   // now used as error counter
  if FInFilename = '' then
    Begin
      WriteLn('Error: infile not specified');
      Inc(I);
    End;
  if FOutFilename = '' then
    Begin
      WriteLn('Error: outfile not specified');
      Inc(I);
    End;
  if I > 0 then
    Halt(1);
End;

(*Procedure TestStuff;
Var V : TValueAccuracyMinMax;
Begin
  V := TValueAccuracyMinMax.Create(10.0, 9.0, 11.0);
  WriteLn(SizeOf(V));
  WriteLn(V.InstanceSize);
  WriteLn(PtrUInt(V));
  WriteLn(PtrUInt(@V.FValue) - PtrUInt(V));
  WriteLn(PtrUInt(@V.FMin)   - PtrUInt(V));
  WriteLn(PtrUInt(@V.FMax)   - PtrUInt(V));
  WriteLn(Int64(Pointer(V)^));
  WriteLn(V.ClassParent.InstanceSize);
  WriteLn(V.ClassParent.ClassParent.InstanceSize);
  WriteLn(TPersistent.InstanceSize);
  Dump(Pointer(V)^, V.InstanceSize);

  Halt(1);
End;*)

Var ComparisonMeasurement : TComparisonMeasurement;

Begin
  //TestStuff;
  // setup, parse parameters
  SetupInstrumentWrapperFactory;
  SetupMeasurementResultFactory;
  SetupObjectFactory;
  ComparisonMeasurement := TComparisonMeasurement.Create;
  // load measurement procedure
  WriteLn('Loading ',ComparisonMeasurement.FInFilename);
  try
    ComparisonMeasurement.Load;
  except
    on E : Exception do
      Begin
        WriteLn(E.Message);
        Halt(1);
      End;
  End;
  // check
  WriteLn('Checking comparison description');
  if ComparisonMeasurement.CheckComparison > 0 then
    Halt(1);
  WriteLn('Information:');
  WriteLn('  Instruments    ',Length(ComparisonMeasurement.FComparison.FInstruments));
  WriteLn('  Sets           ',ComparisonMeasurement.FComparison.FProcedure.GetNumSets);
  WriteLn('  Testpoints     ',ComparisonMeasurement.FComparison.FProcedure.GetNumTestpoints);

  // contact and initialize instruments
  WriteLn('Initializing instruments');
  ComparisonMeasurement.Initialize;

  // run measurement procedure
  WriteLn('Running measurement procedure');
  ComparisonMeasurement.Run;

  // disconnect from instruments
  WriteLn('Disconnecting from instruments');
  ComparisonMeasurement.Disconnect;

  // save results
  WriteLn('Saving to ',ComparisonMeasurement.FOutFilename);
  ComparisonMeasurement.Save;
  ComparisonMeasurement.FComparison.FProcedure.PrintMeasurementsByTestPoint;

  // cleanup
  ComparisonMeasurement.Free;
End.

