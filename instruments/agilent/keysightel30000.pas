(**
 * Driver for Keysight EL30000 DC Electronic Loads
 *
 * Supported Devices:
 *  - EL33133A single-input DC electronic load: 150V, 40A, 250W
 •  - EL34143A single-input DC electronic load: 150V, 60A, 350W
 •  - EL34243A dual-input DC electronic load: 150V, 60A, 300W; total 600W
 *)
Unit KeysightEL30000;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, Instrument, RemoteInstrument;

Type
  TChannel         = 1..2;
  TChannelSet      = set of TChannel;
  TLoadFunction    = (lfCurrent,lfVoltage,lfPower,lfResistance);
  TSenseMode       = (smLocal,smRemote);
  TTransientMode   = (tmContinuous,tmPulse,tmToggle,tmList);
  TSourceTransMode = (stmFixed, stmStep, stmList);

Const
  CLoadFunction         : Array[TLoadFunction]    of String = ('CURRENT', 'VOLTAGE', 'POWER', 'RESISTANCE');
  CLoadFunctionShort    : Array[TLoadFunction]    of String = ('CURR', 'VOLT', 'POW', 'RES');
  CSenseMode            : Array[TSenseMode]       of String = ('INTERNAL','EXTERNAL');
  CSenseModeShort       : Array[TSenseMode]       of String = ('INT','EXT');
  CTransientMode        : Array[TTransientMode]   of String = ('CONTINUOUS', 'PULSE', 'TOGGLE', 'LIST');
  CTransientModeShort   : Array[TTransientMode]   of String = ('CONT', 'PULS', 'TOGG', 'LIST');
  CSourceTransMode      : Array[TSourceTransMode] of String = ('FIXED', 'STEP', 'LIST');
  CSourceTransModeShort : Array[TSourceTransMode] of String = ('FIX', 'STEP', 'LIST');

Type

  { TKeysightEL30000 }

  TKeysightEL30000 = class(TRemoteInstrument)
  private
    FIdentity : TDynStringArray;
  protected
    { internal functions }
    // check if *IDN? string is a supported device
    Function  IsSupportedDevice(IdnArr:TDynStringArray) : Boolean; virtual;
    Function  GetChannelList(Ch:TChannel) : String;
    Function  GetChannelList(Ch:TChannelSet) : String;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    { device function }
    Procedure Reset;
    Procedure SetBeeper(Enable:Boolean);
    Function  GetNextError : String;
    Procedure GetNextError(Out Code:Integer;Out Msg : String);
    { Display }
    Procedure SetDisplayState(Enable:Boolean);
    Function  GetDisplayState : Boolean;
    Procedure SetDisplayText(St:String);
    Procedure ClearDisplay;
    Procedure SetDisplayView(Num:Integer);
    // Load Function Selection
    Procedure SetLoadFunction(Ch:TChannel;LoadFunction:TLoadFunction);
    // Load Setting (depending on selected function)
    Procedure SetCurrent         (Ch:TChannel;Current:Double);
    Procedure SetCurrentRange    (Ch:TChannel;Current:Double);
    Procedure SetCurrentTransient(Ch:TChannel;Current:Double);
    Procedure SetCurrentTransMode(Ch:TChannel;TransMode:TSourceTransMode);
    Procedure SetVoltage         (Ch:TChannel;Voltage:Double);
    Procedure SetVoltageRange    (Ch:TChannel;Voltage:Double);
    Procedure SetResistance      (Ch:TChannel;Resistance:Double);
    Procedure SetResistanceRange (Ch:TChannel;Resistance:Double);
    Procedure SetPower           (Ch:TChannel;Power:Double);
    Procedure SetPowerRange      (Ch:TChannel;Power:Double);
    // Input Terminal settings
    Function  GetSenseMode(Ch : TChannel) : TSenseMode;
    Procedure SetSenseMode(Ch : TChannelSet; SenseMode : TSenseMode);
    Procedure EnableOutput(Ch : TChannelSet; State : Boolean);
    Procedure SetOutputShort(Ch : TChannelSet; Enable:Boolean);
    // (Immediate) Measurements
    Function  MeasureVoltage(Ch:TChannel)    : Double;
    Function  MeasureVoltage(Ch:TChannelSet) : TDynDoubleArray;
    Function  MeasureCurrent(Ch:TChannel)    : Double;
    Function  MeasureCurrent(Ch:TChannelSet) : TDynDoubleArray;
    Function  MeasurePower  (Ch:TChannel)    : Double;
    Function  MeasurePower  (Ch:TChannelSet) : TDynDoubleArray;
    Function  MeasureArrayVoltage(Ch:TChannelSet) : TDynDoubleArray;
    Function  MeasureArrayCurrent(Ch:TChannelSet) : TDynDoubleArray;
    Function  MeasureArrayPower  (Ch:TChannelSet) : TDynDoubleArray;
    // Sense & Sweep & Initiate & Fetch
    Procedure SenseCurrent(Ch:TChannelSet;Enable:Boolean);
    Procedure SenseVoltage(Ch:TChannelSet;Enable:Boolean);
    Procedure SetSweepPoints(Ch:TChannelSet;Points:Integer);
    Procedure SetSweepInterval(Ch:TChannelSet;Interval:Double);
    Procedure InitiateAcquire(Ch:TChannelSet);
    Procedure TriggerAcquire (Ch:TChannelSet);
    Function  FetchArrayVoltage(Ch:TChannelSet) : TDynDoubleArray;
    Function  FetchArrayCurrent(Ch:TChannelSet) : TDynDoubleArray;
    Function  FetchArrayPower  (Ch:TChannelSet) : TDynDoubleArray;
    // Settings for transient loads
    Procedure SetTransientMode      (Ch:TChannelSet;Mode:TTransientMode);
    Procedure SetTransientCount     (Ch:TChannelSet;Count:Integer);
    Procedure SetTransientDutyCycle (Ch:TChannelSet;DutyCycle:Double);
    Procedure SetTransientFrequency (Ch:TChannelSet;Frequency:Double);
    Procedure SetTransientPulseWidth(Ch:TChannelSet;PulseWidth:Double);
    Procedure InitiateTransient(Ch:TChannelSet);
    Procedure InitiateTransientContinuous(Ch:TChannelSet;Enable:Boolean);
    Procedure TriggerTransient (Ch:TChannelSet);

(*    class Function GetRanges(AInstrument:String) : TRangesQuantity; override;*)
  End;

Implementation

{ TKeysightEL30000 }

(**
 * Constructor
 *
 * Checks whether the connected device is a Keysight EL30000 DC Electronic Load
 * and throws an Exception if not.
 *)
Constructor TKeysightEL30000.Create(ADeviceCommunicator: IDeviceCommunicator);
Var Identity : String;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  FIdentity := SplitStr(',',Identity);
  if not IsSupportedDevice(FIdentity) then
    raise Exception.Create('Device '''+Identity+''' is not a supported device');
End;

Destructor TKeysightEL30000.Destroy;
Begin
  Inherited Destroy;
End;

Function TKeysightEL30000.IsSupportedDevice(IdnArr : TDynStringArray) : Boolean;
Begin
  Result := (Length(IdnArr) = 4) and
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'EL33133A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'EL34143A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'EL34243A'));
End;

Function  TKeysightEL30000.GetChannelList(Ch:TChannel) : String;
Begin
  Result := '(@' + IntToStr(Ch) + ')';
End;

Function  TKeysightEL30000.GetChannelList(Ch:TChannelSet) : String;
Var ChI : TChannel;
Begin
  if Ch = [] then
    raise Exception.Create('The channel set must be non-empty');
  Result := '(@';
  For ChI in Ch do
    Result := Result + IntToStr(ChI)+',';
  SetLength(Result, Length(Result)-1);  // cut off trailing ','
  Result := Result + ')';
End;

(**
 * Reset to default settings
 *
 * see [EL30000Prg] p. 70
 *)
Procedure TKeysightEL30000.Reset;
Begin
  FDeviceCommunicator.Send('*RST');
End;

(**
 * Enable/disable the beeper
 *
 * see [EL30000Prg] p. 150
 *)
Procedure TKeysightEL30000.SetBeeper(Enable : Boolean);
Begin
  FDeviceCommunicator.Send('SYSTEM:BEEPER:STATE ' + Select(Enable,'ON','OFF'));
End;

(**
 * Query returns the electronic load error queue of up to 20 errors
 *
 * see [EL30000Prg] p. 117
 *)
Function TKeysightEL30000.GetNextError : String;
Begin
  Result := FDeviceCommunicator.Query('SYSTEM:ERROR?');
End;

Procedure TKeysightEL30000.GetNextError(Out Code : Integer; Out Msg : String);
Var St : String;
    I  : Integer;
Begin
  St := GetNextError;   // returns e.g. '-410,"Query INTERRUPTED"' or '+0,"No error"'
  I := Pos(',',St);
  if I = 0 then
    Begin
      Code := StrToInt(St);
      Msg  := '';
      // raise Exception.Create('Cannot parse return value of GetNextError '''+St+'''');
    End
  else
    Begin
      Code := StrToInt(Copy(St,1,I-1));
      Msg  := Copy(St,I+2,Length(St)-I-2);
    End;
End;

(**
 * Turn the front-panel display off or on
 *
 * see [EL30000Prg] p.62
 *)
Procedure TKeysightEL30000.SetDisplayState(Enable : Boolean);
Begin
  FDeviceCommunicator.Send('DISPLAY:STATE ' + Select(Enable,'ON','OFF'));
End;

(**
 * Check if the front-panel display is turned off or on
 *
 * see [EL30000Prg] p.62
 *)
Function TKeysightEL30000.GetDisplayState : Boolean;
Begin
  Result := (FDeviceCommunicator.Query(':DISPLAY:STATE?') = '1');
End;

(**
 * Display a message of up to 30 characters on the front panel.
 *
 * Additional characters are truncated.
 *
 * see [EL30000Prg] p.62
 *)
Procedure TKeysightEL30000.SetDisplayText(St : String);
Begin
  FDeviceCommunicator.Send('DISPLAY:TEXT "' + St + '"');
End;

(**
 * Clear the message displayed on the front panel
 *
 * see [EL30000Prg] p.62
 *)
Procedure TKeysightEL30000.ClearDisplay;
Begin
  FDeviceCommunicator.Send('DISPLAY:TEXT:CLEAR');
End;

(**
 * Selects 1-channel or 2-channel meter view for EL34243A models
 *
 * @param Num  either 1 (displays one large input channel with one small less
 *             detailed channel) or 2 (displays both input channels)
 *
 * see [EL30000Prg] p.63
 *)
Procedure TKeysightEL30000.SetDisplayView(Num : Integer);
Begin
  FDeviceCommunicator.Send('DISPLAY:VIEW METER'+IntToStr(Num));
End;

// Source Function Selection

(**
 *
 *
 * [SOURce:]FUNCtion CURRent | VOLTage | POWer | RESistance[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 67
 *)
Procedure TKeysightEL30000.SetLoadFunction(Ch : TChannel; LoadFunction : TLoadFunction);
Begin
  FDeviceCommunicator.Send('FUNCTION '+CLoadFunction[LoadFunction]+', '+GetChannelList(Ch));
End;

// Source Setting (depending on selected function)

(**
 * Set load current
 *
 * [SOURce:]CURRent[:LEVel][:IMMediate][:AMPLitude] <current> | MINimum | MAXimum | DEFault[,(@<chanlist>) ]
 *
 * see [EL30000Prg] p. 52
 *
 *)
Procedure TKeysightEL30000.SetCurrent(Ch : TChannel; Current : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:CURRENT '+FloatToStrF(Current,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set current range of the instrument
 *
 * Instrument    |     EL33133A        |     EL34143A         |      EL34243A
 * --------------+---------------------+----------------------+---------------------
 * High range    |  0.010A  .. 40.80A  |  0.0120A .. 61.200A  |  0.0120A .. 61.200A
 * Medium range  |         ---         |  0.0020A ..  6.120A  |  0.0020A ..  6.120A
 * Low range     |  0.001A  ..  4.08A  |  0.0002A ..  0.612A  |  0.0002A ..  0.612A
 *
 * [SOURce:]CURRent:RANGe <current> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p.  56
 *)
Procedure TKeysightEL30000.SetCurrentRange(Ch : TChannel; Current : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:CURRENT:RANGE '+FloatToStrF(Current,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the transient load current
 *
 * [SOURce:]CURRent:TLEVel <transient level> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 58
 *)
Procedure TKeysightEL30000.SetCurrentTransient(Ch : TChannel; Current : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:CURRENT:TLEVEL '+FloatToStrF(Current,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the transient mode
 *
 * The command determines what happens to the input current when the transient
 * system is initiated and triggered.
 *
 * Mode  | Description
 * ------+---------------------------------------------------------------------
 * FIXED | Nothing happens. The input current remains at its immediate value.
 * STEP  | The input goes to the triggered level when a trigger occurs.
 * LIST  | The input follows the list values when a trigger occurs.
 *
 * [SOURce:]CURRent:MODE FIXed | STEP | LIST[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 53
 *)
Procedure TKeysightEL30000.SetCurrentTransMode(Ch : TChannel; TransMode : TSourceTransMode);
Begin
  FDeviceCommunicator.Send('SOURCE:CURRENT:MODE '+CSourceTransMode[TransMode]+', '+GetChannelList(Ch));
End;

(**
 * Set the load voltage
 *
 * [SOURce:]VOLTage[:LEVel][:IMMediate][:AMPLitude] <voltage>| MINimum | MAXimum | DEFault[,(@<chanlist>)]
 *
 * see [EL30000Prg] p. 128
 *)
Procedure TKeysightEL30000.SetVoltage(Ch : TChannel; Voltage : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:VOLTAGE '+FloatToStrF(Voltage,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the instrument load voltage range
 *
 * Instrument    |     EL33133A       |   EL34143A, EL34243A
 * --------------+--------------------+-----------------------
 * High range    |  0.020V .. 153.0V  |  0.015V .. 153.0V
 * Low range     |  0.005V ..  15.3V  |  0.003V ..  15.3V
 *
 * [SOURce:]VOLTage:RANGe <voltage> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 131
 *)
Procedure TKeysightEL30000.SetVoltageRange(Ch : TChannel; Voltage : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:VOLTAGE:RANGE '+FloatToStrF(Voltage,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the load resistance
 *
 * [SOURce:]RESistance[:LEVel][:IMMediate][:AMPLitude] <resistance> | MINimum | MAXimum | DEFault[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 98
 *)
Procedure TKeysightEL30000.SetResistance(Ch : TChannel; Resistance : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:RESISTANCE '+FloatToStrF(Resistance,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the instrument load resistance range
 *
 * Instrument    |     EL33133A       |   EL34143A, EL34243A
 * --------------+--------------------+-----------------------
 * High range    |  100.00Ω .. 4000Ω  |  100.00Ω .. 4000Ω
 * Mediumrange   |   10.00Ω .. 1250Ω  |   10.00Ω .. 1250Ω
 * Low range     |    0.08Ω ..   30Ω  |    0.05Ω ..   30Ω
 *
 * [SOURce:]RESistance:RANGe <resistance> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 100
 *)
Procedure TKeysightEL30000.SetResistanceRange(Ch : TChannel; Resistance : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:RESISTANCE:RANGE '+FloatToStrF(Resistance,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the load power
 *
 * [SOURce:]POWer[:LEVel][:IMMediate][:AMPLitude] <power>| MINimum | MAXimum | DEFault[,(@<chanlist>)]
 *
 * see [EL30000Prg] p. 93
 *)
Procedure TKeysightEL30000.SetPower(Ch : TChannel; Power : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:POWER '+FloatToStrF(Power,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the instrument load power range
 *
 * Instrument    |     EL33133A      |     EL34143A       |      EL34243A
 * --------------+-------------------+--------------------+---------------------
 * High range    |  1.50W .. 255.0W  |  2.00W .. 357.00W  |  2.00W .. 306.00W
 * Medium range  |  0.15W ..  25.5W  |  0.30W ..  35.70W  |  0.30W ..  30.60W
 * Low range     |  0.02W ..   5.1W  |  0.01W ..   8.16W  |  0.01W ..   7.14W
 *
 * [SOURce:]POWer:RANGe <power> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 95
 *)
Procedure TKeysightEL30000.SetPowerRange(Ch : TChannel; Power : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:POWER:RANGE '+FloatToStrF(Power,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

// Input Terminal settings

(**
 * Get the setting of local or remote sensing
 *
 * [SOURce:]VOLTage:SENSe[:SOURce] INTernal | EXTernal[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 134
 *)
Function TKeysightEL30000.GetSenseMode(Ch : TChannel) : TSenseMode;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('SOURCE:VOLTAGE:SENSE? '+GetChannelList(Ch));
  For Result in TSenseMode do
    if St = CSenseModeShort[Result] then
      Exit;
  raise Exception.Create('Invalid return string '''+St+'''');
End;

(**
 * Select local or remote sensing
 *
 * [SOURce:]VOLTage:SENSe[:SOURce] INTernal | EXTernal[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 134
 *)
Procedure TKeysightEL30000.SetSenseMode(Ch : TChannelSet; SenseMode : TSenseMode);
Begin
  FDeviceCommunicator.Send('SOURCE:VOLTAGE:SENSE '+CSenseMode[SenseMode]+', '+GetChannelList(Ch));
End;

(**
 * Switch inputs/outputs on or off
 *
 * INPut[:STATe] ON | OFF | 1 | 0[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 75
 *)
Procedure TKeysightEL30000.EnableOutput(Ch : TChannelSet; State : Boolean);
Begin
  FDeviceCommunicator.Send('OUTPUT:STATE '+Select(State,'ON','OFF')+', '+GetChannelList(Ch));
End;

(**
 * Simulate a short circuit on the input of the electronic load.
 *
 * INPut:SHORt[:STATe] ON | OFF | 1 | 0
 *
 * see [EL30000Prg] p. 80
 *)
Procedure TKeysightEL30000.SetOutputShort(Ch : TChannelSet; Enable : Boolean);
Begin
  FDeviceCommunicator.Send('OUTPUT:SHORT '+Select(Enable,'ON','OFF')+', '+GetChannelList(Ch));
End;

// (Immediate) Measurements

(**
 * Measure the voltage of one channel
 *
 * MEASure[:SCALar]:VOLTage[:DC]? [ CH1 | CH2] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 90
 *)
Function TKeysightEL30000.MeasureVoltage(Ch : TChannel) : Double;
Var St : String;
    J  : Integer;
Begin
  St := FDeviceCommunicator.Query('MEASURE:VOLTAGE? '+GetChannelList(Ch));
  Val(St,Result,J);
  if J <> 0 then
    raise Exception.CreateFmt('Invalid floating point number ''%s'' at position %d',[St,J]);
End;

(**
 * Measure the voltage of multiple channels
 *
 * MEASure[:SCALar]:VOLTage[:DC]? [ CH1 | CH2] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 90
 *)
Function TKeysightEL30000.MeasureVoltage(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:VOLTAGE? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Measure the current of one channel
 *
 * MEASure[:SCALar]:CURRent[:DC]? [ CH1 | CH2] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 90
 *)
Function TKeysightEL30000.MeasureCurrent(Ch : TChannel) : Double;
Var St : String;
    J  : Integer;
Begin
  St := FDeviceCommunicator.Query('MEASURE:CURRENT? '+GetChannelList(Ch));
  Val(St,Result,J);
  if J <> 0 then
    raise Exception.CreateFmt('Invalid floating point number ''%s'' at position %d',[St,J]);
End;

(**
 * Measure the current of multiple channels
 *
 * MEASure[:SCALar]:CURRent[:DC]? [ CH1 | CH2] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 90
 *)
Function TKeysightEL30000.MeasureCurrent(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:CURRENT? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Measure the power of one channel
 *
 * MEASure[:SCALar]:POWer[:DC]? [ CH1 | CH2] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 90
 *)
Function TKeysightEL30000.MeasurePower(Ch : TChannel) : Double;
Var St : String;
    J  : Integer;
Begin
  St := FDeviceCommunicator.Query('MEASURE:POWER? '+GetChannelList(Ch));
  Val(St,Result,J);
  if J <> 0 then
    raise Exception.CreateFmt('Invalid floating point number ''%s'' at position %d',[St,J]);
End;

(**
 * Measure the power of multiple channels
 *
 *
 * MEASure[:SCALar]:POWer[:DC]? [ CH1 | CH2] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 90
 *)
Function TKeysightEL30000.MeasurePower(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:POWER? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Measure a series of voltages
 *
 * The sampling rate is set by SetSweepInterval.
 * The number of points returned is set by SetSweepPoints.
 *
 * MEASure:ARRay:VOLTage[:DC]? [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 91
 *)
Function TKeysightEL30000.MeasureArrayVoltage(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:ARRAY:VOLTAGE? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Measure a series of currents
 *
 * The sampling rate is set by SetSweepInterval.
 * The number of points returned is set by SetSweepPoints.
 *
 * MEASure:ARRay:CURRent[:DC]? [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 91
 *)
Function TKeysightEL30000.MeasureArrayCurrent(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:ARRAY:CURRENT? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Measure a series of powers
 *
 * The sampling rate is set by SetSweepInterval.
 * The number of points returned is set by SetSweepPoints.
 *
 * MEASure:ARRay:POWer[:DC]? [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 91
 *)
Function TKeysightEL30000.MeasureArrayPower(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:ARRAY:POWER? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

// Sense & Sweep & Initiate & Fetch

(**
 * Enable or disable data logging the current of the specified channels
 *
 * SENSe:FUNCtion:CURRent ON | OFF | 1 | 0[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 103
 *)
Procedure TKeysightEL30000.SenseCurrent(Ch : TChannelSet; Enable : Boolean);
Begin
  FDeviceCommunicator.Send('SENSE:FUNCTION:CURRENT '+Select(Enable,'ON','OFF')+', '+GetChannelList(Ch));
End;

(**
 * Enable or disable data logging the voltage of the specified channels
 *
 * SENSe:FUNCtion:VOLTage ON | OFF | 1 | 0[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 103
 *)
Procedure TKeysightEL30000.SenseVoltage(Ch : TChannelSet; Enable : Boolean);
Begin
  FDeviceCommunicator.Send('SENSE:FUNCTION:VOLTAGE '+Select(Enable,'ON','OFF')+', '+GetChannelList(Ch));
End;

(**
 * Define the number of points in a measurement
 *
 * The number of samples (points) that can be specified depends on the number
 * of measurement parameter selected.
 * You can measure up to four parameters (voltage and current x 2 inputs).
 * 1 parameters: up to 128K points
 * 2 parameters: up to  64K points
 * 3 parameters: up to  32K points
 * 4 parameters; up to  16K points (K = 1024)
 *
 * SENSe:SWEep:POINts <data point> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 106
 *)
Procedure TKeysightEL30000.SetSweepPoints(Ch : TChannelSet; Points : Integer);
Begin
  FDeviceCommunicator.Send('SENSE:SWEEP:POINTS '+IntToStr(Points)+', '+GetChannelList(Ch));
End;

(**
 * Define the time period between samples in seconds
 *
 * Programmed values can range from 5.12 µs to 40,000 s.
 *
 * SENSe:SWEep:TINTerval <time> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 107
 *)
Procedure TKeysightEL30000.SetSweepInterval(Ch : TChannelSet; Interval : Double);
Begin
  FDeviceCommunicator.Send('SENSE:SWEEP:TINTERVAL '+FloatToStrF(Interval,ffFixed,1,8)+', '+GetChannelList(Ch));
End;

(**
 * Initiates the measurement trigger system
 *
 * INITiate[:IMMediate]:ACQuire [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 73
 *)
Procedure TKeysightEL30000.InitiateAcquire(Ch : TChannelSet);
Begin
  FDeviceCommunicator.Send('INITIATE:ACQUIRE '+GetChannelList(Ch));
End;

(**
 * Trigger the measurement immediately
 *
 * TRIGger:ACQuire[:IMMediate] [ (@<chanlist>)]
 *
 * see [EL30000Prg] p. 121
 *)
Procedure TKeysightEL30000.TriggerAcquire(Ch : TChannelSet);
Begin
  FDeviceCommunicator.Send('TRIGGER:ACQUIRE '+GetChannelList(Ch));
End;

(**
 * Return a series of voltage measurements
 *
 * The sampling rate is set by SetSweepInterval.
 * The number of points returned is set by SetSweepPoints.
 *
 * FETCh:ARRay:VOLTage[:DC]? [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 66
 *)
Function TKeysightEL30000.FetchArrayVoltage(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('FETCH:ARRAY:VOLTAGE? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 *
 * Return a series of current measurements
 *
 * The sampling rate is set by SetSweepInterval.
 * The number of points returned is set by SetSweepPoints.
 *
 * FETCh:ARRay:CURRent[:DC]? [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 66
 *)
Function TKeysightEL30000.FetchArrayCurrent(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('FETCH:ARRAY:CURRENT? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Return a series of power measurements
 *
 * The sampling rate is set by SetSweepInterval.
 * The number of points returned is set by SetSweepPoints.
 *
 * FETCh:ARRay:POWer[:DC]? [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 66
 *)
Function TKeysightEL30000.FetchArrayPower(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('FETCH:ARRAY:POWER? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

// Settings for transient loads

(**
 * Set the operating mode of the transient generator
 *
 * Mode       | Description
 * -----------+----------------------------------------------------------------------------
 * CONTinuous | The transient generator puts out a continuous pulse stream.
 * PULSe      | The transient generator puts out a single pulse upon receipt of a trigger.
 * TOGGle     | The transient generator toggles between two levels upon receipt of a trigger.
 * LIST       | The transient generator switch to list mode.
 *
 * [SOURce:]TRANsient:MODE CONTinuous | PULSe | TOGGle | LIST[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 120
 *)
Procedure TKeysightEL30000.SetTransientMode(Ch : TChannelSet; Mode : TTransientMode);
Begin
  FDeviceCommunicator.Send('TRANSIENT:MODE '+CTransientMode[Mode]+', '+GetChannelList(Ch));
End;

(**
 * Ses the number of times that the continuous mode is executed before it is completed.
 *
 * The transient count can be set to any value between 1 and 9999.
 *
 * TODO: MIN, MAX, INFinity
 *
 * [SOURce:]TRANsient:COUNt <value> | MINimum | MAXimum | INFinity[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 119
 *)
Procedure TKeysightEL30000.SetTransientCount(Ch : TChannelSet; Count : Integer);
Begin
  FDeviceCommunicator.Send('TRANSIENT:COUNT '+IntToStr(Count)+', '+GetChannelList(Ch));
End;

(**
 * Set the duty cycle for each of the transients when the generator is in continuous mode
 *
 * The value can range from 1.8% to 98.2%
 *
 * [SOURce:]TRANsient:DCYCle <value> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 119
 *)
Procedure TKeysightEL30000.SetTransientDutyCycle(Ch : TChannelSet; DutyCycle : Double);
Begin
  FDeviceCommunicator.Send('TRANSIENT:DCYCLE '+FloatToStrF(DutyCycle,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the frequency of the generator is in continuous mode
 *
 * The value can range from 0.25 to 10000Hz.
 *
 * [SOURce:]TRANsient:FREQuency <value> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 119
 *)
Procedure TKeysightEL30000.SetTransientFrequency(Ch : TChannelSet; Frequency : Double);
Begin
  FDeviceCommunicator.Send('TRANSIENT:FREQUENCY '+FloatToStrF(Frequency,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Set the pulse width of the transients when the generator is in pulse mode
 *
 * The value can range from 0.0001 to 268,435s.
 *
 * [SOURce:]TRANsient:TWIDth <value> | MINimum | MAXimum[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 120
 *)
Procedure TKeysightEL30000.SetTransientPulseWidth(Ch : TChannelSet; PulseWidth : Double);
Begin
  FDeviceCommunicator.Send('TRANSIENT:TWIDTH '+FloatToStrF(PulseWidth,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Initiate the transient trigger system
 *
 * INITiate[:IMMediate]:TRANsient [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 73
 *)
Procedure TKeysightEL30000.InitiateTransient(Ch : TChannelSet);
Begin
  FDeviceCommunicator.Send('INITIATE:TRANSIENT '+GetChannelList(Ch));
End;

(**
 * Enable/disable continuously initiating the transient trigger system
 *
 * With continuous triggering disabled, the trigger system must be initiated
 * for each trigger using the INITiate:TRANsient command.
 *
 * INITiate:CONTinuous:TRANsient ON | OFF | 1 | 0[, (@<chanlist>)]
 *
 * see [EL30000Prg] p. 74
 *)
Procedure TKeysightEL30000.InitiateTransientContinuous(Ch : TChannelSet; Enable : Boolean);
Begin
  FDeviceCommunicator.Send('INITIATE:CONTINUOUS:TRANSIENT '+Select(Enable,'ON','OFF')+', '+GetChannelList(Ch));
End;

(**
 * Trigger the transient immediately
 *
 * TRIGger:TRANsient[:IMMediate] [(@<chanlist>)]
 *
 * see [EL30000Prg] p. 125
 *)
Procedure TKeysightEL30000.TriggerTransient(Ch : TChannelSet);
Begin
  FDeviceCommunicator.Send('TRIGGER:TRANSIENT '+GetChannelList(Ch));
End;

End.

