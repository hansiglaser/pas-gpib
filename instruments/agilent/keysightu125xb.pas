Unit KeysightU125xB;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

Interface

Uses
  Classes, SysUtils,
  RegExpr,
  PasGpibUtils, DevCom, RemoteInstrument;

Type
  TRotarySwitch      = (rsOff,rsVAC,rsVDCAC,rsmVDCAC,rsResistance,rsDiodeFrequency,rsCapacitanceTemperature,rsuADCAC,rsmAADCAC,rsOut);
  TMeasureQuantity   = (mqNone,mqVoltage,mqCurrent,mqFrequency,mqResistance,mqContinuity,mqConductivity,mqDiode,mqCapacitance,mqTemperature,mqCurrentPercent);
  TMeasureCoupling   = (mcNone,mcDC,mcAC,mcACDC);
  TMeasureTempSensor = (mtNone,mtK,mtJ,mtEnv);
  TMeasureTempUnit   = (muNone,muCelsius,muFahrenheit);
  TMeasureLoopRange  = (mlNone,ml4_20mA,ml0_20mA);

  { TMeasureStatus }

  TMeasureStatus = record
    DynamicRecording : Boolean;
    Relative         : Boolean;
    dBm              : Boolean;   // TODO: could also be 'V' for dBV
    Unknown4         : Char;
    PeakHold         : Boolean;
    Unknown6         : Char;
    Unknown7         : Char;
    DataHold         : Boolean;
    Unknown9         : Char;
    Unknown10        : Char;
    Unknown11        : Char;
    Brightness       : Byte;   // display brightness 0-2
    Unknown13        : Char;
    Unknown14        : Char;
    Unknown15        : Char;
    RotarySwitch     : TRotarySwitch;   // rotary switch position 0-8
    OutputOn         : Boolean;
    Unknown18        : Char;
    Unknown19        : Char;
    Unknown20        : Char;
    AutoRange        : Boolean;
    Function ToString : String;
    Function ToStringLong : String;
  End;

  { TMeasureConfig }

  TMeasureConfig = record
    //RotarySwitch:TRotarySwitch;
    Quantity   : TMeasureQuantity;
    Coupling   : TMeasureCoupling;   // only relevant for mqVoltage and mqCurrent
    TempSensor : TMeasureTempSensor; // only relevant for mqTemperature
    TempUnit   : TMeasureTempUnit;   // only relevant for mqTemperature
    LoopRange  : TMeasureLoopRange;  // only relevant for mqCurrentPercent
    Range      : Double;
    Digit      : Double;
    Function ToString : String;
  End;

Const
  CRotarySwitch              : Array[TRotarySwitch]      of String = ('','V AC','V DC/AC/ACDC','mV DC/AC/ACDC','Resistance/Continuity/Conductance','Diode/Frequency','Capacitance/Temperature','µA DC/AC/ACDC','mA/A DC/AC/ACDC','Square Wave Out');
  CMeasureQuantity           : Array[TMeasureQuantity]   of String = ('None','Voltage','Current','Frequency','Resistance','Continuity','Conducitivity','Diode','Capacitance','Temperature','Percent');
  CMeasureQuantityUnitSymbol : Array[TMeasureQuantity]   of String = ('','V','A','Hz','Ω','Ω','S','V','F','°C','%');
  CMeasureCoupling           : Array[TMeasureCoupling]   of String = ('','DC','AC','ACDC');
  CMeasureTempSensor         : Array[TMeasureTempSensor] of String = ('','K','J','ENV');
  CMeasureLoopRange          : Array[TMeasureLoopRange]  of String = ('','4-20mA','0-20mA');
  CMeasureTempUnit           : Array[TMeasureTempUnit]   of String = ('','°C','°F');

Type
  { TKeysightU125xB }

  TKeysightU125xB = class(TRemoteInstrument)
  protected
    { internal functions }
    // check if *IDN? string is a supported device
    Function  IsSupportedDevice(IdnArr:TDynStringArray) : Boolean; virtual;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    { device function }
    Procedure Reset;
    Function  GetMeasureStatus : TMeasureStatus;
    Function  GetMeasureConfig(AChannel : Byte = 1) : TMeasureConfig;
    Procedure SetMeasureConfig;
    Function  GetBatteryLevel : Double;
    Function  GetVersion : String;
    Function  GetError : Integer;
    Function  Measure(AChannel : Byte = 1) : Double;
  private
    class Function MeasureConfigString2Record(ASt : String; Out AMeasureConfig : TMeasureConfig) : Boolean;
    class Function MeasureStatusString2Record(ASt : String; Out AMeasureStatus : TMeasureStatus) : Boolean;
  public
    class Procedure TestMeasureConfigString2Record;
  End;

Implementation

Constructor TKeysightU125xB.Create(ADeviceCommunicator : IDeviceCommunicator);
Var Identity : String;
    IdnArr   : TDynStringArray;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;   // e.g. 'Keysight Technologies,U1253B,MY57210700,V3.04'
  IdnArr := SplitStr(',',Identity);
  if not IsSupportedDevice(IdnArr) then
    raise Exception.Create('Device '''+Identity+''' is not a supported device');
End;

Destructor TKeysightU125xB.Destroy;
Begin
  Inherited Destroy;
End;

Function TKeysightU125xB.IsSupportedDevice(IdnArr : TDynStringArray) : Boolean;
Begin
  Result := (Length(IdnArr) = 4) and
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'U1251B')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'U1252B')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'U1253B'));
End;

(**
 * Reset to default settings
 *
 * This causes a full reboot of the device. Be sure to set the device
 * communicator timeout long enough.
 *)
Procedure TKeysightU125xB.Reset;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('*RST');
  WriteLn(St);
End;

Function TKeysightU125xB.GetMeasureStatus : TMeasureStatus;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('STAT?');
//  WriteLn(St);
  if not MeasureStatusString2Record(St, Result) then
    raise Exception.Create('Can''t interpret measurement status '''+St+'''');
End;

Function TKeysightU125xB.GetMeasureConfig(AChannel : Byte) : TMeasureConfig;
Var St : String;
Begin
  Result.Quantity := mqNone;
  St := FDeviceCommunicator.Query('CONF?' + Select(AChannel <> 1,' @'+IntToStr(AChannel),''));
//  WriteLn(St);
  // an error is returned if the secondary is queried without being active (and the device beeps)
  // in wave output mode it also replies with *E on such a query
  if St = '*E' then
    Exit;
  if not MeasureConfigString2Record(St, Result) then
    raise Exception.Create('Can''t interpret measurement configuration '''+St+'''');
End;

Procedure TKeysightU125xB.SetMeasureConfig;
Begin

End;

Function TKeysightU125xB.GetBatteryLevel : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('SYST:BATT?'));
End;

Function TKeysightU125xB.GetVersion : String;
Begin
  Result := FDeviceCommunicator.Query('SYST:VERS?');
End;

Function TKeysightU125xB.GetError : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('SYST:ERR?'));
End;

Function TKeysightU125xB.Measure(AChannel : Byte) : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('FETC?' + Select(AChannel <> 1,' @'+IntToStr(AChannel),'')));    // 'READ?' is identical but doesn't support other channels
End;

Class Function TKeysightU125xB.MeasureStatusString2Record(ASt : String; Out AMeasureStatus : TMeasureStatus) : Boolean;

  Function Char2RotarySwitch(ACh:Char):TRotarySwitch;
  Begin
    if (ACh < '0') or (ACh > '8') then
      raise Exception.Create('Invalid rotary switch position '''+ACh+'''');
    Result := TRotarySwitch(Ord(rsVAC) + (Ord(ACh)-Ord('0')));
  End;

Begin
  FillChar(AMeasureStatus, SizeOf(AMeasureStatus), 0);
  // e.g., '"000000I00312L00004001"'
  if (Length(ASt) <> 23) or (ASt[1] <> '"') or (ASt[Length(ASt)] <> '"') then
    raise Exception.Create('Can''t interpret status '''+ASt+''': missing ''"''s or wrong length');
  AMeasureStatus.DynamicRecording := (ASt[ 2] = '1');
  AMeasureStatus.Relative         := (ASt[ 3] = '1');
  AMeasureStatus.dBm              := (ASt[ 4] = 'M');
  AMeasureStatus.Unknown4         := ASt[ 5];
  AMeasureStatus.PeakHold         := (ASt[ 6] = '1');
  AMeasureStatus.Unknown6         := ASt[ 7];
  AMeasureStatus.Unknown7         := ASt[ 8];
  AMeasureStatus.DataHold         := (ASt[ 9] = '1');
  AMeasureStatus.Unknown9         := ASt[10];
  AMeasureStatus.Unknown10        := ASt[11];
  AMeasureStatus.Unknown11        := ASt[12];
  AMeasureStatus.Brightness       := StrToInt(ASt[13]);
  AMeasureStatus.Unknown13        := ASt[14];
  AMeasureStatus.Unknown14        := ASt[15];
  AMeasureStatus.Unknown15        := ASt[16];
  AMeasureStatus.RotarySwitch     := Char2RotarySwitch(ASt[17]);
  AMeasureStatus.OutputOn         := (ASt[18] = '1');
  AMeasureStatus.Unknown18        := ASt[19];
  AMeasureStatus.Unknown19        := ASt[20];
  AMeasureStatus.Unknown20        := ASt[21];
  AMeasureStatus.AutoRange        := (ASt[22] = '1');
  Result := True;
End;

Class Function TKeysightU125xB.MeasureConfigString2Record(ASt : String; Out
  AMeasureConfig : TMeasureConfig) : Boolean;

  Function Str2Coupling(ASubStr:String):TMeasureCoupling;
  Begin
    if      ASubStr = ''     then Exit(mcDC)
    else if ASubStr = 'AC'   then Exit(mcAC)
    else if ASubStr = 'ACDC' then Exit(mcACDC)
    else
      raise Exception('Unknown coupling string '''+ASubStr+'''');
  End;

  Function Str2TempSensor(ASubStr:String):TMeasureTempSensor;
  Begin
    if      ASubStr = ''    then Exit(mtNone)
    else if ASubStr = 'K'   then Exit(mtK)
    else if ASubStr = 'J'   then Exit(mtJ)
    else if ASubStr = 'ENV' then Exit(mtEnv)
    else
      raise Exception('Unknown temperature sensor string '''+ASubStr+'''');
  End;

  Function Str2TempUnit(ASubStr:String):TMeasureTempUnit;
  Begin
    if      ASubStr = ''    then Exit(muNone)
    else if ASubStr = 'CEL' then Exit(muCelsius)
    else if ASubStr = 'FAR' then Exit(muFahrenheit)
    else
      raise Exception('Unknown temperature unit string '''+ASubStr+'''');
  End;

  Function Str2LoopRange(ASubStr:String):TMeasureLoopRange;
  Begin
    if      ASubStr = ''       then Exit(mlNone)
    else if ASubStr = '4-20mA' then Exit(ml4_20mA)
    else if ASubStr = '0-20mA' then Exit(ml0_20mA)
    else
      raise Exception('Unknown temperature unit string '''+ASubStr+'''');
  End;

Var RE : TRegExpr;
    //I  : Integer;
Begin
  FillChar(AMeasureConfig, SizeOf(AMeasureConfig), 0);
  // e.g., "VOLT +5.00000000E+00,+1.00000000E-04"
  // e.g., "VOLT:AC +5.00000000E+00,+1.00000000E-04"
  // e.g., "VOLT:ACDC +5.00000000E+00,+1.00000000E-04"
  // e.g., "VOLT +5.00000000E-02,+1.00000000E-06"
  // e.g., "VOLT:AC +5.00000000E-02,+1.00000000E-06"
  // e.g., "VOLT:ACDC +5.00000000E-02,+1.00000000E-06"
  // e.g., "RES +5.00000000E+08,+1.00000000E+04"
  // e.g., "CONT +5.00000000E+02,+1.00000000E-02"
  // e.g., "COND +5.00000000E-07,+1.00000000E-11"
  // e.g., "DIOD"
  // e.g., "PRES 1"      // frequency, /1 or /100 predivider, TODO: how to find out whether we get Hz, ms, %?
  // e.g., "CAP +1.00000000E-08,+1.00000000E-12"
  // e.g., "TEMP:K CEL"
  // e.g., "TEMP:ENV CEL"    // "CONF? @3", environment
  // e.g., "CURR +5.00000000E-04,+1.00000000E-08"
  // e.g., "CURR:AC +5.00000000E-04,+1.00000000E-08"
  // e.g., "CURR:ACDC +5.00000000E-04,+1.00000000E-08"
  // e.g., "CURR +5.00000000E-02,+1.00000000E-06"
  // e.g., "CPER:4-20mA +5.00000000E-02,+1.00000000E-06"
  RE := TRegExpr.Create('^"([A-Z]+)(:([0-9A-Za-z+-]+))?( ([0-9A-Z]+))?( ([-0-9E.+]+),([-0-9E.+]+))?"$');
  if not RE.Exec(ASt) then
    Begin
      //WriteLn('Can''t match ''',ASt,'''');
      RE.Free;
      Exit(False);
    End;

//  Write(RE.SubExprMatchCount,': ');
//  For I := 0 to RE.SubExprMatchCount do
//    Write('''',RE.Match[I],''', ');
//  WriteLn;

  if RE.Match[1] = 'VOLT' then
    // RE.Match = ('"VOLT:AC +5.00000000E+00,+1.00000000E-04"', 'VOLT', ':AC', 'AC', '', '', ' +5.00000000E+00,+1.00000000E-04', '+5.00000000E+00', '+1.00000000E-04')
    AMeasureConfig.Quantity := mqVoltage
  else if RE.Match[1] = 'CURR' then
    // RE.Match = ('"CURR:ACDC +5.00000000E-04,+1.00000000E-08"', 'CURR', ':ACDC', 'ACDC', '', '', ' +5.00000000E-04,+1.00000000E-08', '+5.00000000E-04', '+1.00000000E-08')
    AMeasureConfig.Quantity := mqCurrent
  else if RE.Match[1] = 'RES' then
    // RE.Match = ('"RES +5.00000000E+08,+1.00000000E+04"', 'RES', '', '', '', '', ' +5.00000000E+08,+1.00000000E+04', '+5.00000000E+08', '+1.00000000E+04')
    AMeasureConfig.Quantity := mqResistance
  else if RE.Match[1] = 'CONT' then
    // RE.Match = ('"CONT +5.00000000E+02,+1.00000000E-02"', 'CONT', '', '', '', '', ' +5.00000000E+02,+1.00000000E-02', '+5.00000000E+02', '+1.00000000E-02')
    AMeasureConfig.Quantity := mqContinuity
  else if RE.Match[1] = 'COND' then
    // RE.Match = ('"COND +5.00000000E-07,+1.00000000E-11"', 'COND', '', '', '', '', ' +5.00000000E-07,+1.00000000E-11', '+5.00000000E-07', '+1.00000000E-11')
    AMeasureConfig.Quantity := mqConductivity
  else if RE.Match[1] = 'DIOD' then
    // RE.Match = ('"DIOD"', 'DIOD', '', '', '', '', '', '', '')
    AMeasureConfig.Quantity := mqDiode
  else if RE.Match[1] = 'PRES' then
    // RE.Match = ('"PRES 1"', 'PRES', '', '', ' 1', '1', '', '', '')
    AMeasureConfig.Quantity := mqFrequency
  else if RE.Match[1] = 'CAP' then
    // RE.Match = ('"CAP +1.00000000E-08,+1.00000000E-12"', 'CAP', '', '', '', '', ' +1.00000000E-08,+1.00000000E-12', '+1.00000000E-08', '+1.00000000E-12')
    AMeasureConfig.Quantity := mqCapacitance
  else if RE.Match[1] = 'TEMP' then
    // RE.Match = ('"TEMP:K CEL"', 'TEMP', ':K', 'K', ' CEL', 'CEL', '', '', '')
    AMeasureConfig.Quantity := mqTemperature
  else if RE.Match[1] = 'CPER' then
    // RE.Match = ('"CPER:4-20mA +5.00000000E-02,+1.00000000E-06"', 'CPER', ':4-20mA', '4-20mA', '', '', ' +5.00000000E-02,+1.00000000E-06', '+5.00000000E-02', '+1.00000000E-06')
    AMeasureConfig.Quantity := mqCurrentPercent
  else
    raise Exception.Create('Unknown quantity '''+RE.Match[1]+''' in '''+ASt+'''');

  if AMeasureConfig.Quantity in [mqVoltage,mqCurrent] then
    AMeasureConfig.Coupling := Str2Coupling(RE.Match[3]);
  if AMeasureConfig.Quantity in [mqVoltage,mqCurrent,mqResistance,mqContinuity,mqConductivity,mqCapacitance,mqCurrentPercent] then
    Begin
      AMeasureConfig.Range := StrToFloat(RE.Match[7]);
      AMeasureConfig.Digit := StrToFloat(RE.Match[8]);
    End;
  if AMeasureConfig.Quantity = mqFrequency then
    AMeasureConfig.Range := StrToFloat(RE.Match[5]);
  if AMeasureConfig.Quantity = mqTemperature then
    Begin
      AMeasureConfig.TempSensor := Str2TempSensor(RE.Match[3]);
      AMeasureConfig.TempUnit   := Str2TempUnit  (RE.Match[5]);
    End;
  if AMeasureConfig.Quantity = mqCurrentPercent then
    AMeasureConfig.LoopRange := Str2LoopRange(RE.Match[3]);

  RE.Free;
  Result := True;
//  WriteLn(AMeasureConfig.ToString);
End;

Class Procedure TKeysightU125xB.TestMeasureConfigString2Record;
Var MeasureConfig : TMeasureConfig;
    Result        : Boolean;
Begin
  Result := MeasureConfigString2Record('"VOLT +5.00000000E+00,+1.00000000E-04"',         MeasureConfig);
  Result := MeasureConfigString2Record('"VOLT:AC +5.00000000E+00,+1.00000000E-04"',      MeasureConfig);
  Result := MeasureConfigString2Record('"VOLT:ACDC +5.00000000E+00,+1.00000000E-04"',    MeasureConfig);
  Result := MeasureConfigString2Record('"VOLT +5.00000000E-02,+1.00000000E-06"',         MeasureConfig);
  Result := MeasureConfigString2Record('"VOLT:AC +5.00000000E-02,+1.00000000E-06"',      MeasureConfig);
  Result := MeasureConfigString2Record('"VOLT:ACDC +5.00000000E-02,+1.00000000E-06"',    MeasureConfig);
  Result := MeasureConfigString2Record('"RES +5.00000000E+08,+1.00000000E+04"',          MeasureConfig);
  Result := MeasureConfigString2Record('"CONT +5.00000000E+02,+1.00000000E-02"',         MeasureConfig);
  Result := MeasureConfigString2Record('"COND +5.00000000E-07,+1.00000000E-11"',         MeasureConfig);
  Result := MeasureConfigString2Record('"DIOD"',                                         MeasureConfig);
  Result := MeasureConfigString2Record('"PRES 1"',                                       MeasureConfig);
  Result := MeasureConfigString2Record('"CAP +1.00000000E-08,+1.00000000E-12"',          MeasureConfig);
  Result := MeasureConfigString2Record('"TEMP:K CEL"',                                   MeasureConfig);
  Result := MeasureConfigString2Record('"TEMP:ENV CEL"',                                 MeasureConfig);
  Result := MeasureConfigString2Record('"CURR +5.00000000E-04,+1.00000000E-08"',         MeasureConfig);
  Result := MeasureConfigString2Record('"CURR:AC +5.00000000E-04,+1.00000000E-08"',      MeasureConfig);
  Result := MeasureConfigString2Record('"CURR:ACDC +5.00000000E-04,+1.00000000E-08"',    MeasureConfig);
  Result := MeasureConfigString2Record('"CURR +5.00000000E-02,+1.00000000E-06"',         MeasureConfig);
  Result := MeasureConfigString2Record('"CPER:4-20mA +5.00000000E-02,+1.00000000E-06"',  MeasureConfig);
End;

{ TMeasureStatus }

Function TMeasureStatus.ToString : String;
Begin
  Result :=            'DynamicRecording: ' + Select(DynamicRecording,'on','off');
  Result := Result + ', Relative: '         + Select(Relative,'on','off');
  Result := Result + ', dBm: '              + Select(dBm,'on','off');
  Result := Result + ', PeakHold: '         + Select(PeakHold,'on','off');
  Result := Result + ', DataHold: '         + Select(DataHold,'on','off');
  Result := Result + ', Brightness: '       + IntToStr(Brightness);
  Result := Result + ', RotarySwitch: '     + CRotarySwitch[RotarySwitch];
  Result := Result + ', OutputOn: '         + Select(OutputOn,'on','off');
  Result := Result + ', AutoRange: '        + Select(AutoRange,'on','off');
End;

Function TMeasureStatus.ToStringLong : String;
Begin
  Result :=          'Status:' + LineEnding;
  Result := Result + '  DynamicRecording = ' + Select(DynamicRecording,'on','off') + LineEnding;
  Result := Result + '  Relative         = ' + Select(Relative,'on','off')         + LineEnding;
  Result := Result + '  dBm              = ' + Select(dBm,'on','off')              + LineEnding;
  Result := Result + '  Unknown4         = ' + Unknown4                            + LineEnding;
  Result := Result + '  PeakHold         = ' + Select(PeakHold,'on','off')         + LineEnding;
  Result := Result + '  Unknown6         = ' + Unknown6                            + LineEnding;
  Result := Result + '  Unknown7         = ' + Unknown7                            + LineEnding;
  Result := Result + '  DataHold         = ' + Select(DataHold,'on','off')         + LineEnding;
  Result := Result + '  Unknown9         = ' + Unknown9                            + LineEnding;
  Result := Result + '  Unknown10        = ' + Unknown10                           + LineEnding;
  Result := Result + '  Unknown11        = ' + Unknown11                           + LineEnding;
  Result := Result + '  Brightness       = ' + IntToStr(Brightness)                + LineEnding;
  Result := Result + '  Unknown13        = ' + Unknown13                           + LineEnding;
  Result := Result + '  Unknown14        = ' + Unknown14                           + LineEnding;
  Result := Result + '  Unknown15        = ' + Unknown15                           + LineEnding;
  Result := Result + '  RotarySwitch     = ' + CRotarySwitch[RotarySwitch]         + LineEnding;
  Result := Result + '  OutputOn         = ' + Select(OutputOn,'on','off')         + LineEnding;
  Result := Result + '  Unknown18        = ' + Unknown18                           + LineEnding;
  Result := Result + '  Unknown19        = ' + Unknown19                           + LineEnding;
  Result := Result + '  Unknown20        = ' + Unknown20                           + LineEnding;
  Result := Result + '  AutoRange        = ' + Select(AutoRange,'on','off')        + LineEnding;
End;

{ TMeasureConfig }

Function TMeasureConfig.ToString : String;
Begin
//  RotarySwitch  : TRotarySwitch;
  Result := CMeasureQuantity[Quantity];
  if Quantity in [mqVoltage,mqCurrent] then
    Result := Result + ' ('+CMeasureCoupling[Coupling]+')';
  if Quantity in [mqVoltage,mqCurrent,mqResistance,mqContinuity,mqConductivity,mqCapacitance,mqCurrentPercent] then
    Begin
      Result := Result + ' Range: '+FloatToStrSI(Range,FormatSettings)+CMeasureQuantityUnitSymbol[Quantity]+',';
      Result := Result + ' Digit: '+FloatToStrSI(Digit,FormatSettings)+CMeasureQuantityUnitSymbol[Quantity];
    End;
  if Quantity = mqFrequency then
    Result := Result + ' Range: '+FloatToStr(Range);
  if Quantity = mqTemperature then
    Begin
      Result := Result + ' Sensor Type '+CMeasureTempSensor[TempSensor];
      Result := Result + ' Unit '+CMeasureTempUnit[TempUnit];
    End;
  if Quantity = mqCurrentPercent then
    Result := Result + ' Loop Range '+CMeasureLoopRange[LoopRange];
End;

End.

