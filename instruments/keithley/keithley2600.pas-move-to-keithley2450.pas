Unit Keithley2600;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, RemoteInstrument;

Type
  TModel          = (mdUnknown,md2602A);
  TDigits         = (dg4_5,dg5_5,dg6_5);
  TMeasureFunc    = (mfDCAmps,mfDCVolts,mfOhms,mfWatts);
  TFilterType     = (ftMovingAvg,ftRepeatAvg,ftMedian);
  TAutoZero       = (azOff,azOnce,azAuto);
  TSenseMode      = (smLocal,smRemote);
  TOutputFunction = (ofDCVolts,ofDCAmps);
  TDisplayScreen  = (dsSMUA,dsSMUB,dsBoth,dsUser);

Const
  CModelChannels  : Array[TModel]          of Integer = ({unknown:} 0, {2602A:} 2);
  CDigits         : Array[TDigits]         of String = ('DIGITS_4_5','DIGITS_5_5','DIGITS_6_5');
  CMeasureFunc    : Array[TMeasureFunc]    of String = ('MEASURE_DCAMPS','MEASURE_DCVOLTS','MEASURE_OHMS','MEASURE_WATTS');
  CFilterType     : Array[TFilterType]     of String = ('FILTER_MOVING_AVG','FILTER_REPEAT_AVG','FILTER_MEDIAN');
  CAutoZero       : Array[TAutoZero]       of String = ('AUTOZERO_OFF','AUTOZERO_ONCE','AUTOZERO_AUTO');
  CSenseMode      : Array[TSenseMode]      of String = ('SENSE_LOCAL','SENSE_REMOTE');
  COutputFunction : Array[TOutputFunction] of String = ('OUTPUT_DCVOLTS','OUTPUT_DCAMPS');
  CDisplayScreen  : Array[TDisplayScreen]  of String = ('SMUA','SMUB','SMUA_SMUB','USER');

Type
  TKeithley2600Channel = class;

  { TKeithley2600 }

  (**
   * Keithley 2600 SourceMeter SMU Instruments
   *
   * The communication with the instrument (via GPIB, ...) uses the Test Script
   * Language (TSL) which basically is Lua. Many predefined variables and
   * functions, in an object-oriented manner, are used to control the device,
   * e.g.
   *   smua.source.func = smua.OUTPUT_DCVOLTS
   *   reading = smua.measure.i()
   *   print(reading)
   * All commands and variable names are case sensitive (contrary to normal
   * GPIB!).
   *
   * Since TSL is a full programming language (including if, functions, ...),
   * compared to the pure command style syntax of typical GPIB instruments
   * (e.g. the Agilent 34410A), you might want to program your own scripts
   * in TSL instead of Pascal.
   *
   * The control of the device is split into two classes. TKeithley2600
   * implements all functions which are global for the whole device, like
   * screen functions, errors, ... Since each device can have one or two
   * channels, the individual channel functionality is implemented in
   * TKeithley2600Channel. When creating an object of TKeithley2600 the
   * appropriate number of TKeithley2600 channel objects are created and can
   * be accessed through the Channel[] property.
   *)
  TKeithley2600 = class(TRemoteInstrument)
  protected
    FModel : TModel;
    Procedure CheckModel(IdnArr : TDynStringArray);
  protected
    FChannels : Array of TKeithley2600Channel;
    Procedure CheckChannel(AChannel:Integer);
    Function GetChannel(Index : Integer) : TKeithley2600Channel;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    { device function }
    Procedure Reset;
    Procedure SetBeeper(Enable:Boolean);
    Procedure SetCursor(ARow,ACol:Integer);
    Procedure SetText(AText:String);
    Procedure Print(ARow,ACol:Integer;AText:String);
    Procedure ClearDisplay;
    Procedure SelectDisplay(ADisplayScreen:TDisplayScreen);
    Function  ErrorCount : Integer;
    Procedure GetNextError(Out ACode:Integer;Out AMessage:String;Out ASeverity,ANode : Integer);
    property Channel[Index:Integer] : TKeithley2600Channel read GetChannel;
  End;

  { TKeithley2600Channel }

  TKeithley2600Channel = class
  private
    FInstrument         : TKeithley2600;
    FDeviceCommunicator : IDeviceCommunicator;
    FChannel            : Integer;
  protected
    FName               : String;
  private
    Constructor Create(AInstrument:TKeithley2600;ADeviceCommunicator:IDeviceCommunicator;AChannel:Integer);
  public
    // measure functions
    Procedure SetDisplayDigits(ADigits:TDigits);
    Procedure SetDisplayFunction(AMeasureFunc:TMeasureFunc);
    Procedure SetFilterType(AFilterType:TFilterType);
    Procedure SetFilterCount(AFilterCount:Integer);
    Procedure EnableMeasureFilter(AEnable:Boolean);
    Procedure SetMeasureDelay(ADelay:Double);
    Function  GetMeasureDelay:Double;
    Procedure SetMeasureVoltageAutoRange(AEnable:Boolean);
    Procedure SetMeasureCurrentAutoRange(AEnable:Boolean);
    Procedure SetMeasureVoltageRange(ARange:Double);
    Function  GetMeasureVoltageRange:Double;
    Procedure SetMeasureCurrentRange(ARange:Double);
    Function  GetMeasureCurrentRange:Double;
    Procedure SetNPLC(AIntegrationTime:Double);
    Function  GetNPLC:Double;
    Procedure SetAutoZero(AAutoZero:TAutoZero);
    Procedure Measure(ACount,AIBuf,AVBuf:Integer);
    Function  GetBuffer(ACount,ABuffer:Integer) : TDynDoubleArray;
    Procedure ClearBuffer(ABuffer:Integer);
    Function  Measuring : Boolean;
    // source functions
    Procedure SetSenseMode(ASenseMode:TSenseMode);
    Procedure SetOutputFunction(AFunction:TOutputFunction);
    Procedure SetOutputVoltageLimit(ALimit:Double);
    Function  GetOutputVoltageLimit:Double;
    Procedure SetOutputCurrentLimit(ALimit:Double);
    Function  GetOutputCurrentLimit:Double;
    Procedure SetOutputVoltageAutoRange(AEnable:Boolean);
    Procedure SetOutputCurrentAutoRange(AEnable:Boolean);
    Procedure SetOutputVoltageRange(ARange:Double);
    Function  GetOutputVoltageRange:Double;
    Procedure SetOutputCurrentRange(ARange:Double);
    Function  GetOutputCurrentRange:Double;
    Procedure SetHighCapacitanceMode(AEnable:Boolean);
    Procedure EnableOutput(AEnable:Boolean);
    Procedure SetOutputVoltage(AVoltage:Double);
    Function  GetOutputVoltage:Double;
    Procedure SetOutputCurrent(ACurrent:Double);
    Function  GetOutputCurrent:Double;
    Function  GetCompliance : Boolean;
  End;


Implementation

{ TKeithley2600 }

(**
 * Connect to a Keithley 2600 series Source Meter
 *
 * Currently only the 2602A model is supported. Please add further models if you
 * have access to them.
 *
 * This constructor automatically creates the appropriate number of
 * TKeithley2600Channel objects for the SourceMeter channels.
 *)
Constructor TKeithley2600.Create(ADeviceCommunicator : IDeviceCommunicator);
Var Identity : String;
    IdnArr   : TDynStringArray;
    I        : Integer;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  IdnArr := SplitStr(',',Identity);
//  For I := 0 to Length(IdnArr)-1 do
//    WriteLn(I,': ',IdnArr[I]);
  CheckModel(IdnArr);
  if FModel = mdUnknown then
    raise Exception.Create('Device '''+Identity+''' is not a Keithley 2600 SourceMeter SMU Instrument');
  // setup channels
  SetLength(FChannels,CModelChannels[FModel]);
  For I := 0 to CModelChannels[FModel]-1 do
    FChannels[I] := TKeithley2600Channel.Create(Self,FDeviceCommunicator,I);
End;

Destructor TKeithley2600.Destroy;
Var I : Integer;
Begin
  For I := 0 to Length(FChannels)-1 do
    FChannels[I].Free;
  Inherited Destroy;
End;

(**
 * Reset to default settings
 *
 * [RM] p. 3-22, 19-96
 *)
Procedure TKeithley2600.Reset;
Begin
  FDeviceCommunicator.Send('*RST');
End;

(**
 * Enable/disable the beeper
 *
 * [RM] p. 1-15f, p. 19-23
 *)
Procedure TKeithley2600.SetBeeper(Enable : Boolean);
Begin
  FDeviceCommunicator.Send('beeper.enable=' + Select(Enable,'1','0'));
End;

(**
 * Position cursor at display
 *
 * Allowed ARow: 0, 1
 * Allowed ACol: 0 .. 32
 *
 * [RM] p. 11-4f, 19-49
 *)
Procedure TKeithley2600.SetCursor(ARow, ACol : Integer);
Begin
  FDeviceCommunicator.Send('display.setcursor('+IntToStr(ARow)+','+IntToStr(ACol)+')');
End;

(**
 * Display text on the user screen
 *
 * [RM] p. 19-49f
 *)
Procedure TKeithley2600.SetText(AText : String);
Begin
  FDeviceCommunicator.Send('display.settext("'+AText+'")');
End;

(**
 * Display text on a given position o the user screen
 *)
Procedure TKeithley2600.Print(ARow, ACol : Integer; AText : String);
Begin
  SetCursor(ARow,ACol);
  SetText(AText);
End;

(**
 * Ckear all lines of the display
 *
 * [RM] p. 19-38
 *)
Procedure TKeithley2600.ClearDisplay;
Begin
  FDeviceCommunicator.Send('display.clear()');
End;

(**
 * Select the display screen
 *
 * Use the enum values of TDisplayScreen. These select either the
 * source-measure and compliance of SMUA or SMUB or both, or the user screen.
 *
 * [RM] p. 19-47
 *)
Procedure TKeithley2600.SelectDisplay(ADisplayScreen:TDisplayScreen);
Begin
  FDeviceCommunicator.Send('display.screen = display.'+CDisplayScreen[ADisplayScreen]);
End;

(**
 * Get number of errors in error queue
 *
 * [RM] p. 19-54
 *)
Function TKeithley2600.ErrorCount:Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('print(tostring(errorqueue.count))'));
End;

(**
 * Get number of errors in error queue
 *
 * [RM] p. 19-54f
 *)
Procedure TKeithley2600.GetNextError(Out ACode:Integer;Out AMessage:String;Out ASeverity,ANode:Integer);
  Function MyStrToInt(St:String):Integer;
  Begin
    if St = 'nil' then Exit(0);
    Result := StrToInt(St);
  End;
Begin
  FDeviceCommunicator.Send('errorcode, message, severity, node = errorqueue.next()');
  ACode     := MyStrToInt(FDeviceCommunicator.Query('print(tostring(errorcode))'));
  AMessage  :=            FDeviceCommunicator.Query('print(message)');
  ASeverity := MyStrToInt(FDeviceCommunicator.Query('print(tostring(severity))'));
  ANode     := MyStrToInt(FDeviceCommunicator.Query('print(tostring(node))'));
End;

(**
 * Private method to determine device model
 *)
Procedure TKeithley2600.CheckModel(IdnArr:TDynStringArray);
Begin
  FModel := mdUnknown;
  if Length(IdnArr) <> 4 then Exit;
  if UpperCase(Trim(IdnArr[0])) <> 'KEITHLEY INSTRUMENTS INC.' then Exit;
  Case UpperCase(Trim(IdnArr[1])) of
    'MODEL 2602A': FModel := md2602A;
  End;
End;

(**
 * Private getter for the channel objects
 *)
Function TKeithley2600.GetChannel(Index : Integer) : TKeithley2600Channel;
Begin
  CheckChannel(Index);
  Result := FChannels[Index];
End;

(**
 * Private function to check a channel number
 *)
Procedure TKeithley2600.CheckChannel(AChannel : Integer);
Begin
  if (AChannel < 0) or (AChannel >= Length(FChannels)) then
    raise Exception.CreateFmt('Invalid channel %d',[AChannel]);
End;

{ TKeithley2600Channel }

(**
 * Private constructor, only used by TKeithley2600.Creae()
 *)
Constructor TKeithley2600Channel.Create(AInstrument : TKeithley2600;
  ADeviceCommunicator : IDeviceCommunicator; AChannel : Integer);
Begin
  inherited Create;
  FInstrument         := AInstrument;
  FDeviceCommunicator := ADeviceCommunicator;
  FChannel            := AChannel;
  FName               := 'smu'+Select(FChannel,['a','b']);
End;

(**
 * Select measurement display resolution
 *
 * [RM] p. 6-6, 11-3, 19-50
 *)
Procedure TKeithley2600Channel.SetDisplayDigits(ADigits : TDigits);
Begin
  FDeviceCommunicator.Send('display.'+FName+'.digits = display.'+CDigits[ADigits]);
End;

(**
 * Select measurement display function
 *
 * [RM] p. 6-6, 11-3, 19-50
 *)
Procedure TKeithley2600Channel.SetDisplayFunction(AMeasureFunc : TMeasureFunc);
Begin
  FDeviceCommunicator.Send('display.'+FName+'.measure.func= display.'+CMeasureFunc[AMeasureFunc]);
End;

(**
 * Set measurement filter type
 *
 * [RM] p. 6-12, 19-117
 *)
Procedure TKeithley2600Channel.SetFilterType(AFilterType : TFilterType);
Begin
  FDeviceCommunicator.Send(FName+'.measure.filter.type = '+CFilterType[AFilterType]);
End;

(**
 * Set number of measured readings to yield one filtered measurement
 *
 * [RM] p. 6-12, 19-116
 *)
Procedure TKeithley2600Channel.SetFilterCount(AFilterCount : Integer);
Begin
  FDeviceCommunicator.Send(FName+'.measure.filter.count = '+IntToStr(AFilterCount));
End;

(**
 * Enable/disable filtered measurements
 *
 * [RM] p. 6-12, 19-116
 *)
Procedure TKeithley2600Channel.EnableMeasureFilter(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.measure.filter.enable = '+Select(AEnable,'1','0'));
End;

(**
 * Set an additional settling time before taking a measurement
 *
 * ADelay is in Seconds.
 *
 * Set ADelay = -1.0 to use automatic delay values
 *
 * [RM] p. 4-12, 9-11, 19-115
 *)
Procedure TKeithley2600Channel.SetMeasureDelay(ADelay : Double);
Begin
  FDeviceCommunicator.Send(FName+'.measure.delay = '+FloatToStrF(ADelay,ffExponent,12,2));
End;

(**
 * Query measurement delay
 *)
Function TKeithley2600Channel.GetMeasureDelay : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.measure.delay)'));
End;

(**
 * Enable/disable auto-range for voltage measurements
 *
 * [RM] p. 3-10, 5-5, 6-5, 19-112
 *)
Procedure TKeithley2600Channel.SetMeasureVoltageAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.measure.autorangev = '+Select(AEnable,'1','0'));
End;

(**
 * Enable/disable auto-range for current measurements
 *
 * [RM] p. 3-10, 5-5, 6-5, 19-112
 *)
Procedure TKeithley2600Channel.SetMeasureCurrentAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.measure.autorangei = '+Select(AEnable,'1','0'));
End;

(**
 * Set the measurement range for voltage measurements.
 *
 * Allowed values for model 2601A/2602A:
 *    100mV, 1V, 6V, 40V
 *
 * Note: In voltage souce mode this range is locked to the source voltage range.
 *
 * [RM] p. 3-10, 6-2ff, 19-120
 *)
Procedure TKeithley2600Channel.SetMeasureVoltageRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FName+'.measure.rangev = '+FloatToStrF(ARange,ffFixed,1,3));
End;

Function TKeithley2600Channel.GetMeasureVoltageRange : Double;
Begin
    Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.measure.rangev)'));
End;

(**
 * Set the measurement range for current measurements.
 *
 * Allowed values for model 2601A/2602A:
 *    100nA, 1uA, 10uA, 100uA, 1mA, 10mA, 100mA, 1A, 3A
 *
 * Note: In current souce mode this range is locked to the source current range.
 *
 * [RM] p. 3-10, 6-2ff, 19-120
 *)
Procedure TKeithley2600Channel.SetMeasureCurrentRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FName+'.measure.rangei = '+FloatToStrF(ARange,ffExponent,2,2));
End;

Function TKeithley2600Channel.GetMeasureCurrentRange : Double;
Begin
    Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.measure.rangei)'));
End;

(**
 * Set Integration aperture for measurements as multiple of power line cycles
 *
 * Range: 0.001 to 25
 *
 * [RM] p. 6-6, 19-119
 *)
Procedure TKeithley2600Channel.SetNPLC(AIntegrationTime : Double);
Begin
  FDeviceCommunicator.Send(FName+'.measure.nplc = '+FloatToStrF(AIntegrationTime,ffExponent,5,2));
End;

Function TKeithley2600Channel.GetNPLC : Double;
Begin
    Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.measure.nplc)'));
End;

(**
 * Enable/disable automatic reference measurements and command an immediate
 * reference measurement
 *
 * [RM] p. 3-7f, 19-113
 *)
Procedure TKeithley2600Channel.SetAutoZero(AAutoZero:TAutoZero);
Begin
  FDeviceCommunicator.Send(FName+'.measure.autozero = '+FName+'.'+CAutoZero[AAutoZero]);
End;

(**
 * Initiate a measurement
 *
 * This starts a "overlapped" measurements. That means that it is performed in
 * the background and the communication interface is still functional. This
 * allows to poll the status registers and see if the measurement already has
 * finished (see Measuring()).
 *
 * If using the non-"overlapped", synchronous measurement, there would be no
 * way to find out if it has finished, except with a very long timeout of
 * FDeviceCommunicator.
 *
 * [RM] p. 7-6f, 19-114, 19-119
 *)
Procedure TKeithley2600Channel.Measure(ACount, AIBuf, AVBuf : Integer);
Begin
  FDeviceCommunicator.Send(FName+'.measure.count = '+IntToStr(ACount));
  FDeviceCommunicator.Send(FName+'.measure.overlappediv('+FName+'.nvbuffer'+IntToStr(AIBuf)+', '+FName+'.nvbuffer'+IntToStr(AVBuf)+')');
End;

(**
 * Retrieve measurement results from buffer
 *
 * TODO: use format.data = format.REAL64, format.byteorder = format.LITTLEENDIAN
 *
 * [RM] Sec. 7, p. 19-95
 *)
Function TKeithley2600Channel.GetBuffer(ACount, ABuffer : Integer) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('printbuffer(1, '+IntToStr(ACount)+', '+FName+'.nvbuffer'+IntToStr(ABuffer)+'.readings)');
  Result := SplitDouble(',',St);
End;

(**
 * Clear a buffer
 *
 * [RM] p. 7-6f, 19-127
 *)
Procedure TKeithley2600Channel.ClearBuffer(ABuffer:Integer);
Begin
  FDeviceCommunicator.Send(FName+'.nvbuffer'+IntToStr(ABuffer)+'.clear()');
End;

(**
 * Determine if an "overlapped" measurement is currently in progress
 *
 * [RM] p. 19-174
 *)
Function TKeithley2600Channel.Measuring:Boolean;
Begin
  Result := (StrToInt(FDeviceCommunicator.Query('print(tostring(status.operation.instrument.'+FName+'.condition))')) and $10 <> 0);
End;

(**
 * Set sense mode to local (2-wire) or remote (4-wire)
 *
 * [RM] p. 2-9, 3-10ff, 19-130
 *)
Procedure TKeithley2600Channel.SetSenseMode(ASenseMode : TSenseMode);
Begin
  FDeviceCommunicator.Send(FName+'.sense = '+FName+'.'+CSenseMode[ASenseMode]);
End;

(**
 * Set source function to voltage or current
 *
 * [RM] p. 3-10, 19-133
 *)
Procedure TKeithley2600Channel.SetOutputFunction(AFunction : TOutputFunction);
Begin
  FDeviceCommunicator.Send(FName+'.source.func = '+FName+'.'+COutputFunction[AFunction]);
End;

(**
 * Set maximum source voltage for operation in current source mode
 *
 * [RM] p. 3-10f, 19-135
 *)
Procedure TKeithley2600Channel.SetOutputVoltageLimit(ALimit : Double);
Begin
  FDeviceCommunicator.Send(FName+'.source.limitv= '+FloatToStrF(ALimit,ffExponent,8,2));
End;

Function TKeithley2600Channel.GetOutputVoltageLimit : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.source.limitv)'));
End;

(**
 * Set maximum source current for operation in voltage source mode
 *
 * [RM] p. 3-10f, 19-135
 *)
Procedure TKeithley2600Channel.SetOutputCurrentLimit(ALimit : Double);
Begin
  FDeviceCommunicator.Send(FName+'.source.limiti= '+FloatToStrF(ALimit,ffExponent,8,2));
End;

Function TKeithley2600Channel.GetOutputCurrentLimit : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.source.limiti)'));
End;

(**
 * Enable/disable auto-range for voltage source
 *
 * [RM] p. 3-10, 5-5, 6-5, 19-130
 *)
Procedure TKeithley2600Channel.SetOutputVoltageAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.source.autorangev = '+Select(AEnable,'1','0'));
End;

(**
 * Enable/disable auto-range for current source
 *
 * [RM] p. 3-10, 5-5, 6-5, 19-130
 *)
Procedure TKeithley2600Channel.SetOutputCurrentAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.source.autorangei = '+Select(AEnable,'1','0'));
End;

(**
 * Set the source range for voltage source.
 *
 * Allowed values for model 2601A/2602A:
 *    100mV, 1V, 6V, 40V
 *
 * Note: In voltage souce mode this range is locked to the source voltage range.
 *
 * [RM] p. 3-10, 6-2ff, 19-139
 *)
Procedure TKeithley2600Channel.SetOutputVoltageRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FName+'.source.rangev = '+FloatToStrF(ARange,ffFixed,1,3));
End;

Function TKeithley2600Channel.GetOutputVoltageRange : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.source.rangev)'));
End;

(**
 * Set the source range for current source.
 *
 * Allowed values for model 2601A/2602A:
 *    100nA, 1uA, 10uA, 100uA, 1mA, 10mA, 100mA, 1A, 3A
 *
 * Note: In current souce mode this range is locked to the source current range.
 *
 * [RM] p. 3-10, 6-2ff, 19-139
 *)
Procedure TKeithley2600Channel.SetOutputCurrentRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FName+'.source.rangei = '+FloatToStrF(ARange,ffExponent,2,2));
End;

Function TKeithley2600Channel.GetOutputCurrentRange : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.source.rangei)'));
End;

(**
 * Enable high capacitance mode
 *
 * Note: This also changes auto-range, limit and rage settings!
 *
 * [RM] Sec. 5, p. 19-133
 *)
Procedure TKeithley2600Channel.SetHighCapacitanceMode(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.source.highc = '+Select(AEnable,'1','0'));
End;

(**
 * Switch on/off the source output
 *
 * [RM] p. 3-10f, 19-137
 *)
Procedure TKeithley2600Channel.EnableOutput(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FName+'.source.output = '+Select(AEnable,'1','0'));
End;

(**
 * Set source output voltage
 *
 * [RM] p. 3-10f, 19-134
 *)
Procedure TKeithley2600Channel.SetOutputVoltage(AVoltage : Double);
Begin
  FDeviceCommunicator.Send(FName+'.source.levelv = '+FloatToStrF(AVoltage,ffExponent,8,2));
End;

Function TKeithley2600Channel.GetOutputVoltage : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.source.levelv)'));
End;

(**
 * Set source output current
 *
 * [RM] p. 3-10f, 19-134
 *)
Procedure TKeithley2600Channel.SetOutputCurrent(ACurrent : Double);
Begin
  FDeviceCommunicator.Send(FName+'.source.leveli = '+FloatToStrF(ACurrent,ffExponent,8,2));
End;

Function TKeithley2600Channel.GetOutputCurrent : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FName+'.source.leveli)'));
End;

(**
 * Determine if the current limit in voltage mode or the voltage limit in
 * current mode are reached.
 *
 * [RM] p. 3-5f, 4-2f, 19-132
 *)
Function TKeithley2600Channel.GetCompliance : Boolean;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('print('+FName+'.source.compliance)');
  Result := Trim(St) = 'true';
End;

End.

