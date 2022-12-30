/**
 * InstCmp Parser - Parser
 *
 */

%{

%}

%{

const yymaxdepth = 100;

%}

%token <shortstring> TOK_ID TOK_STRING
%token <integer> TOK_INT
%token <double> TOK_DOUBLE
%token TOK_EOL

%{
//%expect 0
//%debug
%}

%%

input:
  optional_eol comparison;

EOL:
  optional_eol TOK_EOL;

optional_eol:
  optional_eol TOK_EOL | /* empty */;

comparison:
  comparison section |
  comparison subsection |
  comparison instrument |
  comparison setting |
  comparison subsettingidx_double_list |
  comparison subsetting |
  comparison setting_double_list |
  comparison subsetting2idx |
  /* empty */;

section:
  '[' TOK_ID ']' EOL {
    //WriteLn('Section '+$2);
    NewSection    := $2;
    NewSubSection := '';
    NewSubSecNum  := -1;
    if      NewSection = 'Instruments' then
      Begin
        if Length(NewComparison.FInstruments) <> 0 then
          raise Exception.Create('Only one section Instruments allowed');
      End
    else if NewSection = 'Settings' then
      Begin
        // [Instruments] is over
        NewComparison.SetupRanges;
      End
    else if NewSection = 'Testpoints' then
      Begin
        // [Settings] is over
      End
    else if NewSection = 'Procedure' then
      Begin
        // [Testpoints] is over
      End
    else if NewSection = 'Results' then
      Begin
        // [Procedure] is over
      End
    else
      raise Exception.Create('Invalid section '+NewSection);
  };

subsection:
  '(' TOK_ID TOK_INT ')' EOL {
    //WriteLn('Subsection '+$2,' ',$3);
    NewSubSection := $2;
    NewSubSecNum  := $3;
    if (NewSection = 'Procedure') and (NewSubSection = 'Set') then
      Begin
        // prepare for reading measurement procedure sets
        if not assigned(NewComparison.FProcedure) then
          Begin
            // first measurement procedure sets
            if NewSubSecNum <> 0 then
              raise Exception.Create('Have to start with Set 0, not '+IntToStr(NewSubSecNum));
            NewComparison.FProcedure := TComparisonProcedure.Create;
            if assigned(NewCompSet) then
              raise Exception.Create('Can''t have an existing comparison set before the first definition.');
          End;
        if assigned(NewCompSet) then
          Begin
            // second or later measurement procedure sets
            if not assigned(NewCompSet.FTestPoints) then
              raise Exception.Create('Testpoints missing in previous comparison set');
          End;
        if NewSubSecNum <> Length(NewComparison.FProcedure.FSets) then
          raise Exception.Create('Expected Set '+IntToStr(Length(NewComparison.FProcedure.FSets))+', not '+IntToStr(NewSubSecNum));
        // this linked list relies on the fact that NewCompSet is initialized as Nil
        NewCompSet := TComparisonSet.Create(NewComparison, NewCompSet);
        // already add to the procedure, even if it is not yet setup, but since
        // there is no subsection after the last one, that would not be added
        NewComparison.FProcedure.AddSet(NewCompSet);
      End
    else if (NewSection = 'Results') and (NewSubSection = 'Set') then
      Begin
        // prepare for reading measurement result sets
        if not assigned(NewComparison.FProcedure) then
          raise Exception.Create('Results but no procedure defined');
        if NewSubSecNum >= Length(NewComparison.FProcedure.FSets) then
          raise Exception.Create('Set number '+IntToStr(NewSubSecNum)+'  higher than number of sets.');
        // prepare FMeasurements[InstrumentIdx][TestPointIdx]
        SetLength(NewComparison.FProcedure.FSets[NewSubSecNum].FMeasurements, Length(NewComparison.FInstruments));
      End
    else
      raise Exception.Create('Subsection at invalid section '+NewSection);
  };

param_value:
  TOK_ID {
    //WriteLn('Param_value id = ',$1);
    if NewParamValue <> Nil then
      raise Exception.Create('Memory leak: Another parameter but the old ('+NewParamValue.ToSyntax+') was not yet used. Or did you forget to FreeAndNil?');
    NewParamValue := TParamValueIdentifier.Create($1);
  } |
  TOK_INT {                      
    //WriteLn('Param_value int = ',$1);
    if NewParamValue <> Nil then
      raise Exception.Create('Memory leak: Another parameter but the old ('+NewParamValue.ToSyntax+') was not yet used. Or did you forget to FreeAndNil?');
    NewParamValue := TParamValueInteger.Create($1);
  } |
  TOK_DOUBLE {
    //WriteLn('Param_value double = ',$1);
    if NewParamValue <> Nil then
      raise Exception.Create('Memory leak: Another parameter but the old ('+NewParamValue.ToSyntax+') was not yet used. Or did you forget to FreeAndNil?');
    NewParamValue := TParamValueDouble.Create($1);
  } |
  TOK_STRING {
    //WriteLn('Param_value string = ',$1);
    if NewParamValue <> Nil then
      raise Exception.Create('Memory leak: Another parameter but the old ('+NewParamValue.ToSyntax+') was not yet used. Or did you forget to FreeAndNil?');
    NewParamValue := TParamValueString.Create($1);
  };

param:
  TOK_ID '=' param_value {
    // optionally convert TParamValueIdentifier to the correct type
    if NewParamValue is TParamValueIdentifier then
      Begin
        TmpParamValue := Nil;
        if TParamValueIdentifier(NewParamValue).FValue = 'Measure' then
          TmpParamValue := TParamValueFunction.Create(ifMeasure)
        else if TParamValueIdentifier(NewParamValue).FValue = 'Source' then
          TmpParamValue := TParamValueFunction.Create(ifSource)
        else
          raise Exception.Create('Warning: Don''t know how to convert parameter '+$1+'='+NewParamValue.ToSyntax+' to a proper type');
        if assigned(TmpParamValue) then
          Begin
            NewParamValue.Free;
            NewParamValue := TmpParamValue;
          End;
      End;
    //WriteLn('Param ',$1,' = ',NewParamValue.ToSyntax,' of type ',NewParamValue.ClassName);
    if not assigned(NewParams) then
      NewParams := TInstrumentWrapperParams.Create;
    NewParams.FParams.Add($1, NewParamValue);
    NewParamValue := Nil; // don''t free
  };

param_list:
  /* empty */ |
  param {
  } |
  param_list ',' param {
  };

instrument:
  TOK_ID '=' TOK_ID '(' param_list ')' EOL {
    if NewSection <> 'Instruments' then
      raise Exception.Create('Instruments can only be declared in section ''Instruments''');
    //WriteLn('Instrument ',$1,' of type ',$3);
    if assigned(NewParams) then
      //WriteLn(NewParams.ToSyntax)
    else
      NewParams := TInstrumentWrapperParams.Create;
    NewComparison.AddInstrument(NewFactory.CreateWrapper(NewComparison, $3, $1, NewParams));
    NewParams := Nil;
  };

setting:
  TOK_ID '=' param_value EOL {
    if NewSection <> 'Settings' then
      raise Exception.Create('Settings can only be declared in section ''Settings''');
    //WriteLn('Setting ',$1,' = ',NewParamValue.ToSyntax);
    if $1 = 'Quantity' then
      Begin
        if not (NewParamValue is TParamValueIdentifier) then
          raise Exception.Create('Invalid value type '+NewParamValue.ClassName+' for setting '+$1);
        NewComparison.FQuantity := TQuantity(-1);
        For TmpInteger := Integer(Low(TQuantity)) to Integer(High(TQuantity)) do
          if Instrument.CQuantityStr[TQuantity(TmpInteger)] = TParamValueIdentifier(NewParamValue).FValue then
            NewComparison.FQuantity := TQuantity(TmpInteger);
        if NewComparison.FQuantity = TQuantity(-1) then
          raise Exception.Create('Invalid quantity '+TParamValueIdentifier(NewParamValue).FValue);
      End
    else
      raise Exception.Create('Invalid setting '+$1);
    FreeAndNil(NewParamValue);
  };

subsetting:
  TOK_ID '.' TOK_ID '=' param_value EOL {
    //WriteLn('Subsetting ',$1,'.',$3,' = ',NewParamValue.ToSyntax);
    //WriteLn('NewSection = ',NewSection,', NewSubSection = ',NewSubSection);
    if (NewSection = 'Procedure') and (NewSubSection = 'Set') then
      Begin
        if $3 <> 'Range' then
          raise Exception.Create('Invalid subsetting '+$3);
        TmpInstIdx := NewComparison.GetInstrumentIndex($1);
        if TmpInstIdx < 0 then
          raise Exception.Create('Invalid instrument '+$1);
        TmpDouble := GetParamDouble;  // also FreeAndNil''s NewParamValue
        if NewCompSet.FRanges[TmpInstIdx] <> Nil then
          raise Exception.Create('Duplicate range setting for instrument '+$1);
        NewCompSet.FRanges[TmpInstIdx] := NewComparison.GetInstrumentRange(TmpInstIdx, TmpDouble);
        if NewCompSet.FRanges[TmpInstIdx] = Nil then
          raise Exception.Create('Invalid range '+FloatToStr(TmpDouble)+' of instrument '+$1);
      End
    else if (NewSection = 'Results') and (NewSubSection = '') then
      Begin
        if $3 <> 'Identifier' then
          raise Exception.Create('Invalid subsetting '+$3);
        TmpInstIdx := NewComparison.GetInstrumentIndex($1);
        if TmpInstIdx < 0 then
          raise Exception.Create('Invalid instrument '+$1);
        if not (NewParamValue is TParamValueString) then
          raise Exception.Create('Invalid value type '+NewParamValue.ClassName+' for setting '+$1+'.'+$3);
        NewComparison.FInstruments[TmpInstIdx].FIdentifier := (NewParamValue as TParamValueString).FValue;
        FreeAndNil(NewParamValue);
      End
    else
      raise Exception.Create('Subsetting can only be declared in section ''Procedure''');
  }

doubles:
  /* empty */ |
  TOK_INT {
    //WriteLn('doubles int = ',$1);
    SetLength(NewDoubles, Length(NewDoubles)+1);
    NewDoubles[Length(NewDoubles)-1] := $1;
  } |
  TOK_DOUBLE {
    //WriteLn('doubles double = ',$1);
    SetLength(NewDoubles, Length(NewDoubles)+1);
    NewDoubles[Length(NewDoubles)-1] := $1;
  } |
  doubles TOK_INT {
    //WriteLn('doubles + int = ',$2);
    SetLength(NewDoubles, Length(NewDoubles)+1);
    NewDoubles[Length(NewDoubles)-1] := $2;
  } |
  doubles TOK_DOUBLE {
    //WriteLn('doubles + double = ',$2);
    SetLength(NewDoubles, Length(NewDoubles)+1);
    NewDoubles[Length(NewDoubles)-1] := $2;
  };

doubles_list:
  '[' { 
    if Length(NewDoubles) <> 0 then
      raise Exception.Create('Start of a doubles list but the old one was not used.');
  } doubles ']' {
    //Write('double_list =');
    //For TmpInteger := 0 to Length(NewDoubles)-1 do
    //  Write(' ',FloatToStr(NewDoubles[TmpInteger]));
    //WriteLn;
  };
  
setting_double_list:
  TOK_ID '=' doubles_list EOL {
    //WriteLn($1,' = doubles_list');
    if (NewSection = 'Procedure') and (NewSubSection = 'Set') and ($1 = 'Testpoints') then
      Begin
        NewCompSet.FTestPoints := TTestPoints.Create(NewDoubles);
        NewCompSet.FMaxVal     := MaxDoubleValue(NewDoubles);
      End
    else
      raise Exception.Create('Invalid double list '+$1+' in section '+NewSection+' subsection '+NewSubSection);
    SetLength(NewDoubles, 0);
  };

subsettingidx_double_list:
  TOK_ID '.' TOK_ID '[' param_value ']' '=' doubles_list EOL {
    // example: Fluke177_0.Testpoints[0.6] = [0 0.001 0.01 0.1 0.12 0.24 0.36 0.48 0.6]
    //WriteLn('Subsettingidx ',$1,'.',$3,'[',NewParamValue.ToSyntax,']');
    if NewSection = 'Testpoints' then
      Begin
        TmpInstIdx := NewComparison.GetInstrumentIndex($1);
        if TmpInstIdx < 0 then
          raise Exception.Create('Invalid instrument '+$1);
        TmpDouble := GetParamDouble;  // also FreeAndNil''s NewParamValue
        TmpRangeIdx := NewComparison.GetInstrumentRangeIdx(TmpInstIdx, TmpDouble);
        if TmpRangeIdx < 0 then
          raise Exception.Create('Invalid range '+FloatToStr(TmpDouble)+' of instrument '+$1);
        if Length(NewComparison.FInstruments[TmpInstIdx].FTestpoints) = 0 then
          // first list of testpoints --> create array
          SetLength(NewComparison.FInstruments[TmpInstIdx].FTestPoints,Length(NewComparison.FInstruments[TmpInstIdx].FRanges[NewComparison.FQuantity]));
        if assigned(NewComparison.FInstruments[TmpInstIdx].FTestpoints[TmpRangeIdx]) then
          raise Exception.Create('Testpoints for range '+FloatToStr(TmpDouble)+' for instrument '+$1+' already set.');
        NewComparison.FInstruments[TmpInstIdx].FTestpoints[TmpRangeIdx] := TTestPoints.Create(NewDoubles);
        SetLength(NewDoubles, 0);
      End
    else
      raise Exception.Create('Subsettingidx can only be declared in section ''Testpoints''');
  };

subsetting2idx:
  TOK_ID '.' TOK_ID '[' param_value {TmpDouble := GetParamDouble;} ',' param_value ']' '=' TOK_STRING EOL {
    // example: E36313A_Ch23M.Measurements[6,0] = 'TValueAccuracyMinMax(2A5437177FDB53BFE1DF36471C4171BF30D76CEE724D5D3F)+TValueAccuracyMinMax(8DEDB5A0F7C6D0BE4ED965CFD00516BF9C1B4FDBF1EC133F)' # V = -0.001212 ±0.0030004848 32, I = -4E-6 ±0.00008001 32
    TmpDouble2 := GetParamDouble;  // also FreeAndNil''s NewParamValue
    // TmpDouble is the range, TmpDouble2 is the testpoint
    //WriteLn('Subsetting2idx ',$1,'.',$3,'[',TmpDouble,',',TmpDouble2,'] = ''', $11, '''');
    if (NewSection = 'Results') and (NewSubSection = 'Set') and ($3 = 'Measurements') then
      Begin
        TmpInstIdx := NewComparison.GetInstrumentIndex($1);
        if TmpInstIdx < 0 then
          raise Exception.Create('Invalid instrument '+$1);
        TmpRangeIdx := NewComparison.GetInstrumentRangeIdx(TmpInstIdx, TmpDouble);
        if TmpRangeIdx < 0 then
          raise Exception.Create('Invalid range '+FloatToStr(TmpDouble)+' of instrument '+$1);
        NewCompSet := NewComparison.FProcedure.FSets[NewSubSecNum];  // use as temporary abbreviation
        // prepare FMeasurements[InstrumentIdx][TestPointIdx], the out array with InstrumentIdx was already created at subsection entry
        if Length(NewCompSet.FMeasurements[TmpInstIdx]) = 0 then
          // first measurement for this set --> create array
          SetLength(NewCompSet.FMeasurements[TmpInstIdx], Length(NewCompSet.FTestPoints.FValues));
        for TmpInteger := 0 to Length(NewCompSet.FTestPoints.FValues)-1 do
          if SameValue(NewCompSet.FTestPoints.FValues[TmpInteger], TmpDouble2) then
            Begin
              if assigned(NewCompSet.FMeasurements[TmpInstIdx][TmpInteger]) then
                raise Exception('There is already a result for testpoint '+FloatToStr(TmpDouble2));
              NewCompSet.FMeasurements[TmpInstIdx][TmpInteger] := MeasurementResultFactory.CreateResult($11);
              //NewCompSet.FMeasurements[TmpInstIdx][TmpInteger] := TMeasurementResultVI.CreateDecode($11);
            End;
      End
    else
      raise Exception.Create('Subsetting2idx but something is wrong');
  }



/*results:*/


