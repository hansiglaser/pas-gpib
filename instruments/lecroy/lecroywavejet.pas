(**
 * Remote control of LeCroy WaveJet oscilloscopes
 *
 * [RCM] WaveJet 300 Series Oscilloscopes Remote Control Manual, February 2007
 *)
Unit LeCroyWaveJet;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils,
  DevCom, RemoteInstrument;

Type
  TDisplayType   = (dtYT, dtXY, dtXYTriggered);
  TCoupling      = (cpAC1M,cpGND,cpDC1M,cpDC50);
  TTriggerSource = (tsCH1,tsCH2,tsCH3,tsCH4,tsEXT,tsEXT10,tsLine);
  TTriggerType   = (ttEdge,ttPulse,ttPeriod,ttCount,ttTV);
  TTriggerMode   = (tmAuto,tmNormal,tmSingle,tmStop);
  TImageFormat   = (ifTiff,ifBmp,ifPng);

Const
  CDisplayType   : Array[TDisplayType]   of String = ('YT','XY','XYTRG');
  CCoupling      : Array[TCoupling]      of String = ('AC1M','GND','DC1M','DC50');
  CTriggerSource : Array[TTriggerSource] of String = ('CH1','CH2','CH3','CH4','EXT','EXT10','LINE');
  CTriggerType   : Array[TTriggerType]   of String = ('EDGE','PULSE','PERIOD','COUNT','TV');
  CTriggerMode   : Array[TTriggerMode]   of String = ('AUTO','NORMAL','SINGLE','STOP');
  CImageFormat   : Array[TImageFormat]   of String = ('TIFF','BMP','PNG');


Type
  TLeCroyWaveJetChannel = class;
  TLeCroyWaveJetChannelIndex = (CH1,CH2,CH3,CH4,M1);
  TLeCroyWaveJetChannelSet = set of TLeCroyWaveJetChannelIndex;

Const
   CLeCroyWaveJetChannelName : Array[TLeCroyWaveJetChannelIndex] of String = ('C1','C2','C3','C4','M1');
   CLeCroyWaveJetChannelReal = [CH1,CH2,CH3,CH4];

Type
  { TLeCroyWaveJet }

  (**
   * LeCroy WaveJet scopes
   *)
  TLeCroyWaveJet = class(TRemoteInstrument)
  protected
    FValidChannels : TLeCroyWaveJetChannelSet;
    FChannels : Array[TLeCroyWaveJetChannelIndex] of TLeCroyWaveJetChannel;
    Procedure CheckChannel(AChannel:TLeCroyWaveJetChannelIndex);
    Function  GetChannel  (AChannel:TLeCroyWaveJetChannelIndex) : TLeCroyWaveJetChannel;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    // Horizontal
    Function  GetTDiv : Double;
    Procedure SetTDiv(ATDiv:Double);
    // Acquisition
    Procedure SetTriggerMode(AMode:TTriggerMode);
    // Status
    // Cursor
    // Hard Copy
    // Data transfer
    Function Screen(AImageFormat:TImageFormat) : TDynByteArray;
    // Math
    // Display
    Function  GetDisplayType : TDisplayType;
    Procedure SetDisplayType(ADisplayType:TDisplayType);
    // IEEE 488.1 Emulation
    // Others
    // Measure
    // Automatic Measurements
    // Save/Recall
    Procedure Reset;
    // Trigger
    Procedure SetTriggerSource(ASource:TTriggerSource);
    Procedure SetTriggerType(AType:TTriggerType);
    Procedure SetTriggerSlope(ASlope:Boolean);
    Procedure SetTriggerLevel(ALevel:Double);
    Function  GetTriggerLevel:Double;
    // System commands
    Function  GetDate : String;
    Procedure SetDate(Year,Month,Day,Hour,Minute,Second:Integer);
    property Channel[Index:TLeCroyWaveJetChannelIndex] : TLeCroyWaveJetChannel read GetChannel;
  End;

  { TLeCroyWaveJetChannel }

  TLeCroyWaveJetChannel = class
  private
    FInstrument         : TLeCroyWaveJet;
    FDeviceCommunicator : IDeviceCommunicator;
    FChannel            : TLeCroyWaveJetChannelIndex;
    Procedure CheckRealChannel(AMethod : String);
  protected
    FName               : String;
  private
    Constructor Create(AInstrument:TLeCroyWaveJet;ADeviceCommunicator:IDeviceCommunicator;AChannel:TLeCroyWaveJetChannelIndex);
  public
    // Vertical
    Procedure Trace(ATrace:Boolean);
    Function  GetCoupling : TCoupling;
    Procedure SetCoupling(ACoupling:TCoupling);
    Function  GetVDiv : Double;
    Procedure SetVDiv(AVDiv:Double);
    Procedure SetOffset(AOffset:Double);
    Function  GetOffset : Double;
  End;

Implementation

{ TLeCroyWaveJet }

(**
 * Constructor
 *
 * Checks whether the connected device is a LeCroy WaveJet scope and throws an
 * Exception if not.
 *)
Constructor TLeCroyWaveJet.Create(ADeviceCommunicator: IDeviceCommunicator);
Var Identity : String;
    IdnArr   : Array of String;
    Ch       : TLeCroyWaveJetChannelIndex;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  // LECROY,<model>,<serial_number>,<firmware-level>
  // <model>: 5-digit model ID (e.g., WJ354)
  // <serial_number>: 14-digit serial number (e.g., LCRY0101J00001)
  // <firmware_level>: 1-digit number showing release level, followed by a period, and a 2-digit minor release level (i.e., the format is "x.yy".)
  IdnArr := SplitStr(',',Identity);
  if (Length(IdnArr) <> 4) or
     (IdnArr[0] <> 'LECROY') or
     (Copy(IdnArr[1],1,2) <> 'WJ') then
    raise Exception.Create('Device '''+Identity+''' is not a LeCroy WaveJet oscilloscope');

  // setup channels
  FValidChannels := [CH1,CH2,CH3,CH4,M1];  // TODO: make this depend on the actual model
  For Ch in FValidChannels do
    FChannels[Ch] := TLeCroyWaveJetChannel.Create(Self,FDeviceCommunicator,Ch);
End;

Destructor TLeCroyWaveJet.Destroy;
Begin
  Inherited Destroy;
End;

(**
 * Acquire a screenshot and return its data
 *
 * Currently only the PNG file format is supported.
 *
 * Note: At the scope you can setup that the screen should be inverted. It seems
 * there is no GPIB SCPI command to do that remotely.
 *
 * Warning: Taking a screenshot requires a few seconds, so be sure to set
 * the timeout high enough (e.g. 5s).
 *
 * [RCM] p. 47
 *)
Function TLeCroyWaveJet.Screen(AImageFormat:TImageFormat) : TDynByteArray;
Var St : String;
    L  : LongInt;
Begin
  St := FDeviceCommunicator.Query('TSCRN? '+CImageFormat[AImageFormat]);
  (* The return format is '#8<byte_length><binary_block>'
   * where <byte_length> are 8 digits (with leading zeros) specifying the number
   * of bytes in <binary_block>.
   *)
  if (Length(St) < 10) or (St[1] <> '#') or (St[2] <> '8') then
    raise Exception.Create('Received invalid data format');
  L := StrToInt(Copy(St,3,8));
  if Length(St) <> 10+L then
    raise Exception.Create('Received '+IntToStr(Length(St))+' bytes but expected '+IntToStr(10+L)+' ('+IntToStr(L)+' for image data)');
  // just use the data starting at the 11th byte
  SetLength(Result,Length(St)-10);
  Move(St[11],Result[0],Length(St)-10);
End;

(**
 * Query the timebase
 *
 * Returns the time/div in seconds
 *
 * [TCM] p. 40
 *)
Function TLeCroyWaveJet.GetTDiv : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('TDIV?'));
End;

(**
 * Set the timebase
 *
 * The given ATDiv value will be rounded up to the nearest time base setting.
 *
 * [TCM] p. 40
 *)
Procedure TLeCroyWaveJet.SetTDiv(ATDiv : Double);
Begin
  FDeviceCommunicator.Send('TDIV '+FloatToStrF(ATDiv,ffExponent,2,2));
End;

(**
 * Set trigger/acquisition mode
 *
 * [RCM] p. 46
 *)
Procedure TLeCroyWaveJet.SetTriggerMode(AMode : TTriggerMode);
Begin
  FDeviceCommunicator.Send('TRMD '+CTriggerMode[AMode]);
End;

Function TLeCroyWaveJet.GetDisplayType : TDisplayType;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('XYDS?');
  For Result := Low(TDisplayType) to High(TDisplayType) do
    if St = CDisplayType[Result] then
      Exit;
End;

Procedure TLeCroyWaveJet.SetDisplayType(ADisplayType : TDisplayType);
Begin
  FDeviceCommunicator.Send('XYDS '+CDisplayType[ADisplayType]);
End;

(**
 * Reset to default settings
 *)
Procedure TLeCroyWaveJet.Reset;
Begin
  FDeviceCommunicator.Send('*RST');
End;

(**
 * Set trigger signal source
 *
 * [RCM] p. 48
 *)
Procedure TLeCroyWaveJet.SetTriggerSource(ASource : TTriggerSource);
Begin
  FDeviceCommunicator.Send('TSRC '+CTriggerSource[ASource]);
End;

(**
 * Set trigger type
 *
 * [RCM] p. 49
 *)
Procedure TLeCroyWaveJet.SetTriggerType(AType : TTriggerType);
Begin
  FDeviceCommunicator.Send('TTYP '+CTriggerType[AType]);
End;

(**
 * Set trigger slope
 *
 * true  -> positive
 * false -> negative
 *
 * [RCM] p. 47
 *)
Procedure TLeCroyWaveJet.SetTriggerSlope(ASlope : Boolean);
Begin
  FDeviceCommunicator.Send('TSLP '+Select(ASlope,'POS','NEG'));
End;

(**
 * Set trigger level of the channel set by the trigger source
 *
 * The value is given in Volts.
 *
 * [RCM] p. 42
 *)
Procedure TLeCroyWaveJet.SetTriggerLevel(ALevel : Double);
Begin
  FDeviceCommunicator.Send('TLVL '+FloatToStrF(ALevel,ffExponent,4,2));
End;

(**
 * Query trigger level
 *
 * The return value is given in Volts.
 *
 * [RCM] p. 42
 *)
Function TLeCroyWaveJet.GetTriggerLevel : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('TLVL?'));
End;

(**
 * Query the current system date in the instrument
 *
 * Format is <day>,<month>,<year>,<hour>,<minute>,<second>
 *   <day>    := a number from 1 to 31.
 *   <month>  := {JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC}
 *   <year>   := a number from 2000 to 2099.
 *   <hour>   := a number from 0 to 23.
 *   <minute> := a number from 0 to 59.
 *   <second> := a number from 0 to 59.
 *
 * [RCM] p. 17
 *)
Function TLeCroyWaveJet.GetDate: String;
Begin
  Result := FDeviceCommunicator.Query('DATE?');
End;

(**
 * Set system date of the instrument
 *
 * Year   must be in 2000..2099
 * Month  must be in 1..12
 * Day    must be in 1..31
 * Hour   must be in 0..23
 * Minute must be in 0..59
 * Second must be in 0..59
 *
 * [RCM] p. 17
 *)
Procedure TLeCroyWaveJet.SetDate(Year, Month, Day, Hour, Minute, Second : Integer);
Const MonthStr : Array[1..12] of String[3] = ('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC');
Begin
  FDeviceCommunicator.Send(Format('DATE %d,%s,%d,%d,%d,%d',[Day,MonthStr[Month],Year,Hour,Minute,Second]));;
End;

Function TLeCroyWaveJet.GetChannel(AChannel:TLeCroyWaveJetChannelIndex) : TLeCroyWaveJetChannel;
Begin
  CheckChannel(AChannel);
  Result := FChannels[AChannel];
End;

Procedure TLeCroyWaveJet.CheckChannel(AChannel : TLeCroyWaveJetChannelIndex);
Begin
  if not (AChannel in FValidChannels) then
    raise Exception.CreateFmt('Invalid channel %d',[AChannel]);
End;

{ TLeCroyWaveJetChannel }

Constructor TLeCroyWaveJetChannel.Create(AInstrument : TLeCroyWaveJet; ADeviceCommunicator : IDeviceCommunicator; AChannel : TLeCroyWaveJetChannelIndex);
Begin
  inherited Create;
  FInstrument         := AInstrument;
  FDeviceCommunicator := ADeviceCommunicator;
  FChannel            := AChannel;
  FName               := CLeCroyWaveJetChannelName[AChannel];
End;

(**
 * Enable/disable the channel
 *
 * [PG] p. 267
 *)
Procedure TLeCroyWaveJetChannel.Trace(ATrace:Boolean);
Begin
  CheckRealChannel('SetCoupling');
  FDeviceCommunicator.Send(FName+':TRA '+Select(ATrace,'ON','OFF'));
End;

(**
 * Query current coupling mode
 *
 * [RCM] p. 17
 *)
Function TLeCroyWaveJetChannel.GetCoupling : TCoupling;
Var St : String;
Begin
  CheckRealChannel('GetCoupling');
  St := FDeviceCommunicator.Query(FName+':CPL?');
  For Result := Low(TCoupling) to High(TCoupling) do
    if St = CCoupling[Result] then
      Exit;
End;

(**
 * Query current coupling mode
 *
 * [RCM] p. 17
 *)
Procedure TLeCroyWaveJetChannel.SetCoupling(ACoupling : TCoupling);
Begin
  CheckRealChannel('SetCoupling');
  FDeviceCommunicator.Send(FName+':CPL '+CCoupling[ACoupling]);
End;

(**
 * Query the vertical sensitivity
 *
 * [RCM] p. 53
 *)
Function TLeCroyWaveJetChannel.GetVDiv : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(FName+':VDIV?'));
End;

(**
 * Set the vertical sensitivity
 *
 * The value will be rounded:
 *           AVDiv <= 0.002    2mV/div
 *   0.002 < AVDiv <= 0.005    5mV/div
 *   0.005 < AVDiv <= 0.01    10mV/div
 *   0.01  < AVDiv <= 0.02    20mV/div
 *   0.02  < AVDiv <= 0.05    50mV/div
 *   0.05  < AVDiv <= 0.1    100mV/div
 *   0.1   < AVDiv <= 0.2    200mV/div
 *   0.2   < AVDiv <= 0.5    500mV/div
 *   0.5   < AVDiv <= 1         1V/div
 *   1     < AVDiv <= 2         2V/div
 *   2     < AVDiv <= 5         5V/div
 *   5     < AVDiv             10V/div
 *
 * Probe ratio settings will be used to multiply AVDiv, i.e. the real voltage
 * at the probe's tip is used. E.g., if a 10:1 probe is used, the values are
 * 100V/div to 20mV/div.
 *
 * [RCM] p. 53
 *)
Procedure TLeCroyWaveJetChannel.SetVDiv(AVDiv : Double);
Begin
  FDeviceCommunicator.Send(FName+':VDIV '+FloatToStrF(AVDiv,ffExponent,2,2));
End;

(**
 * Set the vertical position (offset) of the channel
 *
 * AOffset is interpreted in volts.
 *
 * [RCM] p. 33
 *)
Procedure TLeCroyWaveJetChannel.SetOffset(AOffset : Double);
Begin
  FDeviceCommunicator.Send(FName+':OFST '+FloatToStrF(AOffset,ffExponent,4,2));
End;

(**
 * Query the vertical offset
 *
 * [RCM] p. 33
 *)
Function TLeCroyWaveJetChannel.GetOffset:Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(FName+':OFST?'));
End;

Procedure TLeCroyWaveJetChannel.CheckRealChannel(AMethod:String);
Begin
  if not (FChannel in CLeCroyWaveJetChannelReal) then
    Exception.Create(AMethod+' is only allowed for real channels');
End;

End.

