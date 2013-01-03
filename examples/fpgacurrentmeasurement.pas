Program FPGACurrentMeasurement;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, LinuxGPIB, OoGPIB, DevComGPIB, RemoteInstrument, Keithley2010, LeCroyWaveJet, RohdeSchwarzFSEB, Serial, Keyboard, Math;

Type DoubleArray = Array of Double;


Type

  { TMeasurement }

  TMeasurement = class
  private
    FLabel    : String;
    FChannel  : Integer;
    FChannelDelay : Integer;
    FKey      : Boolean;
    FDuration : Double;
    FCount    : Integer;
    FK2010    : TKeithley2010;
    FData     : DoubleArray;
    FMin      : Double;
    FAvg      : Double;
    FMax      : Double;
  public
    Constructor Create(ALabel:String;AChannel:Integer;AChannelDelay:Integer;AKey:Boolean;ADuration:Double;ACount:Integer;AK2010:TKeithley2010);
    Procedure Measure;
    Function Save : String;
  End;

  { TMeasureRowSetup }

  TMeasureRowSetup = class
  private
    FMeasurements : Array of TMeasurement;
  public
    Constructor Create;
    Procedure AddMeasurement(AMeasurement:TMeasurement);
    Procedure Measure;
    Function Save(Prefix:String) : String;
  End;

  { TMeasureMatrixSetup }

  TMeasureMatrixSetup = class
  private
    FMeasurements : Array of TMeasureRowSetup;
    FLabels       : Array of String;
  public
    Constructor Create(AColLabels:Array of String;ARowLabels:Array of String;ARowChannels:Array of Integer;AChannelDelay:Integer; AKey:Boolean;ADuration:Double;ACount:Integer;AK2010:TKeithley2010);
    Procedure Measure;
    Function Save(Prefix:String) : String;
  End;

{ TMeasurement }

Constructor TMeasurement.Create(ALabel: String; AChannel: Integer; AChannelDelay:Integer; AKey: Boolean; ADuration: Double; ACount: Integer; AK2010: TKeithley2010);
Begin
  FLabel    := ALabel;
  FChannel  := AChannel;
  FChannelDelay := AChannelDelay;
  FKey      := AKey;
  FDuration := ADuration;
  FCount    := ACount;
  FK2010    := AK2010;
  SetLength(FData,0);
End;

Procedure TMeasurement.Measure;
Var Done  : Boolean;
    Start : Double;
    N,M   : Integer;
    St    : String;
Begin
  Write('Set channel ',FChannel,'.');
  FK2010.SelectChannel(FChannel);
  Write('  Settling...');
  Sleep(FChannelDelay);
  Write(^M,Space(30));
  SetLength(FData,0);
  Start := Now;
  N     := 0;
  repeat
    Inc(N);
    SetLength(FData,N);
    repeat
      M := 10;
      try
        FData[N-1] := FK2010.FreshData;
        M := -1;  // show that
      except
        on E : Exception do
          Begin
            Dec(M);
            WriteLn(^M,N,': Error: ',E.Message,', ',M,' retries left');
            Sleep(100);
          End;
      End;
    until M <= 0;
    Write(^M,N,': ',FData[N-1]);
    { termination condition }
    if KeyPressed then
      Begin
        RawReadKey;  // eat up the key
        if FKey then
          Done := true
        else
          Begin
            Write(^J,'Do you really want to stop? Type "yes": ');
            ReadLn(St);
            if St <> 'yes' then
              Begin
                WriteLn('continuing');
              End
            else
              Begin
                WriteLn('ok, done');
                Done := true;
              End;
          End;
      End
    else if FDuration > 0.0 then Done := ((Now - Start)*24*3600 > FDuration)
    else if FCount > 0 then Done := (N >= FCount)
    else Done := true;
    if M = 0 then  // M counted to 0, too many retries
      Begin
        Done := true;
        WriteLn('Too many errors, stopping acquiration.');
      End;
  Until Done; // or KeyPressed;  // TODO: remove 'or KeyPressed'
  WriteLn(^M,'Got ',N,' values:',Space(30));
  FMin := MinValue(FData);
  FAvg := Mean(FData);
  FMAx := MaxValue(FData);
  WriteLn('  mean  = ',FAvg);
  WriteLn('  min   = ',FMin);
  WriteLn('  max   = ',FMax);
  WriteLn('  range = ',Double(abs(FMax-FMin)),' = ',Double(abs((FMax-FMin)/FAvg)*100.0):1:3,'%');
End;

Function TMeasurement.Save: String;
Var I : Integer;
Begin
  Result := '"' + FLabel + '",' + IntToStr(Length(FData)) + ',' + FloatToStr(Mean(FData))+','+FloatToStr(MinValue(FData))+','+FloatToStr(MaxValue(FData));
  For I := 0 to Length(FData)-1 do
    Result := Result + ',' + FloatToStr(FData[I]);
End;

{ TMeasureRowSetup }

Constructor TMeasureRowSetup.Create;
Begin
  SetLength(FMeasurements,0);
End;

Procedure TMeasureRowSetup.AddMeasurement(AMeasurement: TMeasurement);
Begin
  SetLength(FMeasurements,Length(FMeasurements)+1);
  FMeasurements[Length(FMeasurements)-1] := AMeasurement;
End;

Procedure TMeasureRowSetup.Measure;
Var I : Integer;
Begin
  For I := 0 to Length(FMeasurements)-1 do
     FMeasurements[I].Measure;
End;

Function TMeasureRowSetup.Save(Prefix:String) : String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to Length(FMeasurements)-1 do
     Result := Result + Prefix + IntToStr(I) + ',' + FMeasurements[I].Save + ^J;
End;

{ TMeasureMatrixSetup }

Constructor TMeasureMatrixSetup.Create(AColLabels: Array Of String; ARowLabels: Array Of String;
  ARowChannels: Array Of Integer; AChannelDelay:Integer; AKey: Boolean;
  ADuration: Double; ACount: Integer; AK2010: TKeithley2010);
Var Row,Col : Integer;
Begin
  SetLength(FLabels,      Length(AColLabels));
  SetLength(FMeasurements,Length(AColLabels));
  For Col := 0 to Length(AColLabels)-1 do
    Begin
      FLabels      [Col] := AColLabels[Col];   // can't simply assign the whole array :-(
      FMeasurements[Col] := TMeasureRowSetup.Create;
      For Row := 0 to Length(ARowLabels)-1 do
        Begin
          FMeasurements[Col].AddMeasurement(TMeasurement.Create(AColLabels[Col] + ':' + ARowLabels[Row],ARowChannels[Row],AChannelDelay,AKey,ADuration,ACount,AK2010));
        End;
    End;
End;

Procedure TMeasureMatrixSetup.Measure;
Var I : Integer;
Begin
  For I := 0 to Length(FMeasurements)-1 do
     Begin
       Write('Starting "',FLabels[I],'": press key when ready');
       RawReadKey;
       WriteLn;
       FMeasurements[I].Measure;
     End;
  WriteLn('Done.');
End;

Function TMeasureMatrixSetup.Save(Prefix:String) : String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to Length(FMeasurements)-1 do
     Result := Result + FMeasurements[I].Save(Prefix + IntToStr(I) + ',');
End;


(****************************************************************************)

Procedure WriteToFile(FileName:String;Contents:String);
Var T : Text;
Begin
  Assign(T,FileName);
  Rewrite(T);
  Write(T,Contents);
  Close(T);
End;

Var DC    : TGPIBCommunicator {TRS232Communicator} ;
    K2010 : TKeithley2010;
    M     : TMeasureMatrixSetup;
    SaveFileName : String;
Begin
  { use command line parameter for a file name }
  SaveFileName := 'measurements.csv';
  if ParamCount = 1 then
    SaveFileName := ParamStr(1);

  { device communicator }
  DC := TGPIBCommunicator.Create('2010');
  DC.SetTimeout(1000000{us});
//DC := TRS232Communicator.Create('/dev/ttyUSB0',19200,8,NoneParity,0,[]);
  { remote instrument }
  K2010 := TKeithley2010.Create(DC);
  { matrix measurement }
  M := TMeasureMatrixSetup.Create(
//    ['Power off'{,'Power on','JTAG connect','JTAG active','Programmed'}],
    ['Reset','CpuEnable','DutDisable','DutEnable'],
    ['1.2V','2.5V','3.3V'],[1,2,3],10000,
    false, 0.0, 50{2500}, K2010);

  M.Measure;
  WriteLn('Writing results to ',SaveFileName);
  WriteToFile(SaveFileName,M.Save(''));

  { free all objects }
  M.Free;
  K2010.Free;
  DC.Free;
End.

