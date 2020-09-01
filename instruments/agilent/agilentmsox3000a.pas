(**
 * Remote control of Agilent MSO-X 3000A oscilloscopes
 *
 * [PG] Keysight InfiniiVision 3000 X-Series Oscilloscopes Programmer's Guide
 *      Version 02.38.0000, June 9, 2015
 *)
Unit AgilentMSOX3000A;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils,
  DevCom, RemoteInstrument;

Type
  TTimebaseMode  = (tmMain, tmWindow, tmXY, tmRoll);
  TCoupling      = (cpAC,cpDC);
  TTriggerSource = (tsCH1,tsCH2,tsCH3,tsCH4,tsDig0,tsDig1,tsDig2,tsDig3,tsDig4,tsDig5,tsDig6,tsDig7,tsDig8,tsDig9,tsDig10,tsDig11,tsDig12,tsDig13,tsDig14,tsDig15,tsEXT,tsLine,tsWGen);
  TTriggerType   = (ttEdge,ttGlitch,ttPattern,ttTV,ttDelay,ttEBurst,ttOr,ttRunt,ttShold,ttTransition,ttSBus1,ttSBus2,ttUSB);
  TTriggerSlope  = (tsNegative, tsPositive, tsEither, tsAlternate);
  TTriggerMode   = (tmAuto,tmNormal);
  TImageFormat   = (ifBMP, ifBMP8bit, ifPNG);
  TImagePalette  = (ipColor, ipGrayscale);

Const
  // use the abbreviated forms because a quera only returns that, then the search in this array is simpler
  CTimebaseMode  : Array[TTimebaseMode]  of String = ('MAIN','WIND','XY','ROLL');
  CCoupling      : Array[TCoupling]      of String = ('AC','DC');
  CTriggerSource : Array[TTriggerSource] of String = ('CHAN1','CHAN2','CHAN3','CHAN4','DIG0','DIG1','DIG2','DIG3','DIG4','DIG5','DIG6','DIG7','DIG8','DIG9','DIG10','DIG11','DIG12','DIG13','DIG14','DIG15','EXT','LINE','WGEN');
  CTriggerType   : Array[TTriggerType]   of String = ('EDGE','GLIT','PAT','TV','DEL','EBUR','OR','RUNT','SHOL','TRAN','SBUS1','SBUS2','USB');
  CTriggerSlope  : Array[TTriggerSlope]  of String = ('NEG', 'POS', 'EITH', 'ALT');
  CTriggerMode   : Array[TTriggerMode]   of String = ('AUTO','NORMAL');
  CImageFormat   : Array[TImageFormat]   of String = ('BMP','BMP8','PNG');
  CImagePalette  : Array[TImagePalette]  of String = ('COL','GRAY');


Type
  TAgilentMSOX3000AChannel = class;
  TAgilentMSOX3000AChannelIndex = (CH1,CH2,CH3,CH4);
  TAgilentMSOX3000AChannelSet = set of TAgilentMSOX3000AChannelIndex;

Const
   CAgilentMSOX3000AChannelName : Array[TAgilentMSOX3000AChannelIndex] of String = ('CHAN1','CHAN2','CHAN3','CHAN4');
   CAgilentMSOX3000AChannelReal = [CH1,CH2,CH3,CH4];

Type
  { TAgilentMSOX3000A }

  (**
   * Agilent InfiniiVision MSO-X scopes
   *)
  TAgilentMSOX3000A = class(TRemoteInstrument)
  protected
    FValidChannels : TAgilentMSOX3000AChannelSet;
    FChannels : Array[TAgilentMSOX3000AChannelIndex] of TAgilentMSOX3000AChannel;
    Procedure CheckChannel(AChannel:TAgilentMSOX3000AChannelIndex);
    Function  GetChannel  (AChannel:TAgilentMSOX3000AChannelIndex) : TAgilentMSOX3000AChannel;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    Function  GetNextError : String;
    Procedure GetNextError(Out Code : Integer; Out Msg : String);
    // Horizontal
    Function  GetTDiv : Double;
    Procedure SetTDiv(ATDiv:Double);
    // Acquisition
    Procedure Single;
    Procedure Run;
    Procedure Stop;
    Procedure SetTriggerMode(AMode:TTriggerMode);
    // Status
    // Cursor
    // Hard Copy
    Procedure SetHardcopyOptions(AInkSaver : Boolean);
    Function  GetHardcopyOptions : String;
    // Data transfer
    Function Screen(AImageFormat : TImageFormat; AImagePalette : TImagePalette) : TDynByteArray;
    // Math
    // Display
    Function  GetTimebaseMode : TTimebaseMode;
    Procedure SetTimebaseMode(ATimebaseMode : TTimebaseMode);
    // IEEE 488.1 Emulation
    // Others
    // Measure
    // Automatic Measurements
    // Save/Recall
    Procedure Reset;
    // Trigger
    Procedure SetTriggerSource(ASource:TTriggerSource);
    Procedure SetTriggerType(AType:TTriggerType);
    Procedure SetTriggerSlope(ASlope:TTriggerSlope);
    Procedure SetTriggerLevel(ALevel:Double);
    Function  GetTriggerLevel:Double;
    // System commands
    Function  GetDate : String;
    Function  GetTime : String;
    Procedure GetDateTime(Out Year, Month, Day, Hour, Minute, Second : Integer);
    Function  GetDateTime : String;
    Procedure SetDateTime(Year,Month,Day,Hour,Minute,Second:Integer);
    property Channel[Index:TAgilentMSOX3000AChannelIndex] : TAgilentMSOX3000AChannel read GetChannel;
  End;

  { TAgilentMSOX3000AChannel }

  TAgilentMSOX3000AChannel = class
  private
    FInstrument         : TAgilentMSOX3000A;
    FDeviceCommunicator : IDeviceCommunicator;
    FChannel            : TAgilentMSOX3000AChannelIndex;
    Procedure CheckRealChannel(AMethod : String);
  protected
    FName               : String;
  private
    Constructor Create(AInstrument:TAgilentMSOX3000A;ADeviceCommunicator:IDeviceCommunicator;AChannel:TAgilentMSOX3000AChannelIndex);
  public
    // Vertical
    Procedure Display(ADisplay:Boolean);
    Function  GetCoupling : TCoupling;
    Procedure SetCoupling(ACoupling:TCoupling);
    Function  GetVDiv : Double;
    Procedure SetVDiv(AVDiv:Double);
    Procedure SetOffset(AOffset:Double);
    Function  GetOffset : Double;
  End;

Implementation

{ TAgilentMSOX3000A }

(**
 * Constructor
 *
 * Checks whether the connected device is a Agilent MSO-X 3000A scope and throws an
 * Exception if not.
 *)
Constructor TAgilentMSOX3000A.Create(ADeviceCommunicator: IDeviceCommunicator);
Var Identity : String;
    IdnArr   : Array of String;
    Ch       : TAgilentMSOX3000AChannelIndex;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  // AGILENT TECHNOLOGIES,<model>,<serial_number>,<firmware-level>
  // <model>: e.g. MSO-X 3024A
  // <serial_number>: 14-digit serial number (e.g., MY12345678)
  // <firmware_level>: 1-digit number showing release level, followed by a period, and a 2-digit minor release level (i.e., the format is "x.yy".)
  IdnArr := SplitStr(',',Identity);
  if (Length(IdnArr) <> 4) or
     (IdnArr[0] <> 'AGILENT TECHNOLOGIES') or
     (Copy(IdnArr[1],1,7) <> 'MSO-X 3') then
    raise Exception.Create('Device '''+Identity+''' is not an Agilent InfiniiVision MSO-X 3000A oscilloscope');

  // setup channels
  FValidChannels := [CH1,CH2,CH3,CH4];  // TODO: make this depend on the actual model
  For Ch in FValidChannels do
    FChannels[Ch] := TAgilentMSOX3000AChannel.Create(Self,FDeviceCommunicator,Ch);
End;

Destructor TAgilentMSOX3000A.Destroy;
Begin
  Inherited Destroy;
End;

(**
 * Get next error
 *
 * [PG] p. 385
 *)
Function TAgilentMSOX3000A.GetNextError : String;
Begin
  Result := FDeviceCommunicator.Query('SYSTEM:ERROR?');
End;

(**
 * Get next error, split in code and message
 *)
Procedure TAgilentMSOX3000A.GetNextError(Out Code : Integer; Out Msg : String);
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
 * Acquire a screenshot and return its data
 *
 * Use the method HardcopySet to set inverted screen, ...
 *
 * Warning: The screenshot data is at least 18kB. Be sure to set the transfer size of
 * the USBTMC device communicator before calling this function.
 *
 * [PG] p. 306
 *)
Function TAgilentMSOX3000A.Screen(AImageFormat:TImageFormat;AImagePalette:TImagePalette) : TDynByteArray;
Var St : String;
    L  : LongInt;
Begin
  St := FDeviceCommunicator.Query(':DISPLAY:DATA? '+CImageFormat[AImageFormat]+', '+CImagePalette[AImagePalette]);
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
 * Set screen shot and hardcopy options
 *
 * [PG] p. 357ff
 *)
Procedure TAgilentMSOX3000A.SetHardcopyOptions(AInkSaver:Boolean);
Begin
  FDeviceCommunicator.Send(':HARDCOPY:INKSAVER '+Select(AInkSaver,'ON','OFF'));
End;

(**
 * Query screenshot and hardcopy settings
 *
 * Returns a string like ':HARD:APR "Network Printer 2";AREA SCR;FACT 0;FFE 0;INKS 0;PAL COL;LAY PORT'
 * with information on the individual hardcopy settings:
 *   AREA: what part of the display area is printed, only SCReen supported
 *   APRinter: active printer
 *   FACTors: controls whether the scale factors are output on the hardcopy dump
 *   FFEed: controls whether a formfeed is output between the screen image and factors of a hardcopy dump
 *   INKSaver: ontrols whether the graticule colors are inverted or not
 *   LAYout: hardcopy layout mode (LANDscape | PORTrait)
 *   PALette: hardcopy palette color (color, grayscale, none)
 *
 * [PG] p. 358f
 *)
Function TAgilentMSOX3000A.GetHardcopyOptions : String;
Begin
  Result := FDeviceCommunicator.Query(':HARDCOPY?');
End;

(**
 * Query the timebase
 *
 * Returns the time/div in seconds
 *
 * [PG] p. 869
 *)
Function TAgilentMSOX3000A.GetTDiv : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(':TIMEBASE:SCALE?'));
End;

(**
 * Set the timebase
 *
 * The given ATDiv value will be rounded up to the nearest time base setting.
 *
 * [PG] p. 869
 *)
Procedure TAgilentMSOX3000A.SetTDiv(ATDiv : Double);
Begin
  FDeviceCommunicator.Send(':TIMEBASE:SCALE '+FloatToStrF(ATDiv,ffExponent,2,2));
End;

(**
 * Acquire a single trigger of data
 *
 * [PG] p. 221
 *)
Procedure TAgilentMSOX3000A.Single;
Begin
  FDeviceCommunicator.Send(':SINGLE');
End;

(**
 * Start repetitive acquisitions
 *
 * [PG] p. 219
 *)
Procedure TAgilentMSOX3000A.Run;
Begin
  FDeviceCommunicator.Send(':RUN');
End;

(**
 * Stop the acquisition
 *
 * [PG] p. 223
 *)
Procedure TAgilentMSOX3000A.Stop;
Begin
  FDeviceCommunicator.Send(':STOP');
End;

(**
 * Set trigger/acquisition mode
 *
 * Agilent calls this "trigger sweep", and it only supports "normal" and "auto",
 * but not "single" and "stop" like LeCroy. Use the methods Single, Run, and
 * Stop instead.
 *
 * [PG] p. 886
 *)
Procedure TAgilentMSOX3000A.SetTriggerMode(AMode : TTriggerMode);
Begin
  FDeviceCommunicator.Send(':TRIGGER:SWEEP '+CTriggerMode[AMode]);
End;

(**
 * Query timebase mode (normal, XY, roll)
 *
 * [PG] p. 865
 *)
Function TAgilentMSOX3000A.GetTimebaseMode : TTimebaseMode;
Var St : String;
Begin
  St := FDeviceCommunicator.Query(':TIMEBASE:MODE?');
  For Result := Low(TTimebaseMode) to High(TTimebaseMode) do
    if St = CTimebaseMode[Result] then
      Exit;
End;

(**
 * Set timebase mode (normal, XY, roll)
 *
 * [PG] p. 865
 *)
Procedure TAgilentMSOX3000A.SetTimebaseMode(ATimebaseMode : TTimebaseMode);
Begin
  FDeviceCommunicator.Send(':TIMEBASE:MODE '+CTimebaseMode[ATimebaseMode]);
End;

(**
 * Reset to default settings
 *)
Procedure TAgilentMSOX3000A.Reset;
Begin
  FDeviceCommunicator.Send('*RST');
End;

(**
 * Set trigger signal source
 *
 * [PG] p. 904
 *)
Procedure TAgilentMSOX3000A.SetTriggerSource(ASource : TTriggerSource);
Begin
  FDeviceCommunicator.Send(':TRIGGER:EDGE:SOURCE '+CTriggerSource[ASource]);
End;

(**
 * Set trigger type
 *
 * [PG] p. 884
 *)
Procedure TAgilentMSOX3000A.SetTriggerType(AType : TTriggerType);
Begin
  FDeviceCommunicator.Send(':TRIGGER:MODE '+CTriggerType[AType]);
End;

(**
 * Set trigger slope
 *
 * [PG] p. 903
 *)
Procedure TAgilentMSOX3000A.SetTriggerSlope(ASlope : TTriggerSlope);
Begin
  FDeviceCommunicator.Send(':TRIGGER:EDGE:SLOPE '+CTriggerSlope[ASlope]);
End;

(**
 * Set trigger level of the channel set by the trigger source
 *
 * The value is given in Volts.
 *
 * [PG] p. 901
 *)
Procedure TAgilentMSOX3000A.SetTriggerLevel(ALevel : Double);
Begin
  FDeviceCommunicator.Send(':TRIGGER:EDGE:LEVEL '+FloatToStrF(ALevel,ffExponent,4,2));
End;

(**
 * Query trigger level
 *
 * The return value is given in Volts.
 *
 * [PG] p. 901
 *)
Function TAgilentMSOX3000A.GetTriggerLevel : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(':TRIGGER:EDGE:LEVEL?'));
End;

(**
 * Query the current system date in the instrument
 *
 * Format is <year>,<month>,<day> with NR1 format each, e.g. '+2020,+8,+31'
 *
 * [PG] p. 851
 *)
Function TAgilentMSOX3000A.GetDate : String;
Begin
  Result := FDeviceCommunicator.Query(':SYSTEM:DATE?');
End;

(**
 * Query the current system time in the instrument
 *
 * Format is <hours>,<minutes>,<seconds> with NR1 format each, e.g. '+2020,+8,+31'
 *
 * [PG] p. 862
 *)
Function TAgilentMSOX3000A.GetTime : String;
Begin
  Result := FDeviceCommunicator.Query(':SYSTEM:TIME?');
End;

(**
 * Query date and time
 *)
Procedure TAgilentMSOX3000A.GetDateTime(Out Year, Month, Day, Hour, Minute, Second : Integer);
Var St  : String;
    Arr : Array of String;
Begin
  St  := GetDate;
  Arr := SplitStr(',',St);
  if Length(Arr) <> 3 then
    raise Exception.Create('Received date in invalid format '''+St+'''');
  Year  := StrToInt(Arr[0]);
  Month := StrToInt(Arr[1]);
  Day   := StrToInt(Arr[2]);
  St  := GetTime;
  Arr := SplitStr(',',St);
  if Length(Arr) <> 3 then
    raise Exception.Create('Received time in invalid format '''+St+'''');
  Hour   := StrToInt(Arr[0]);
  Minute := StrToInt(Arr[1]);
  Second := StrToInt(Arr[2]);
End;

(**
 * Query date and time as string
 *)
Function TAgilentMSOX3000A.GetDateTime : String;
Var Year, Month, Day, Hour, Minute, Second : Integer;
Begin
  GetDateTime(Year, Month, Day, Hour, Minute, Second);
  Result := Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [Year, Month, Day, Hour, Minute, Second]);
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
 * [PG] p. 851 and 862
 *)
Procedure TAgilentMSOX3000A.SetDateTime(Year, Month, Day, Hour, Minute, Second : Integer);
Begin
  FDeviceCommunicator.Send(Format(':SYSTEM:DATE %d,%d,%d',[Day,Month,Year]));;
  FDeviceCommunicator.Send(Format(':SYSTEM:TIME %d,%d,%d',[Hour,Minute,Second]));;
End;

Function TAgilentMSOX3000A.GetChannel(AChannel:TAgilentMSOX3000AChannelIndex) : TAgilentMSOX3000AChannel;
Begin
  CheckChannel(AChannel);
  Result := FChannels[AChannel];
End;

Procedure TAgilentMSOX3000A.CheckChannel(AChannel : TAgilentMSOX3000AChannelIndex);
Begin
  if not (AChannel in FValidChannels) then
    raise Exception.CreateFmt('Invalid channel %d',[AChannel]);
End;

{ TAgilentMSOX3000AChannel }

Constructor TAgilentMSOX3000AChannel.Create(AInstrument : TAgilentMSOX3000A; ADeviceCommunicator : IDeviceCommunicator; AChannel : TAgilentMSOX3000AChannelIndex);
Begin
  inherited Create;
  FInstrument         := AInstrument;
  FDeviceCommunicator := ADeviceCommunicator;
  FChannel            := AChannel;
  FName               := CAgilentMSOX3000AChannelName[AChannel];
End;

(**
 * Enable/disable the channel
 *
 * [PG] p. 267
 *)
Procedure TAgilentMSOX3000AChannel.Display(ADisplay:Boolean);
Begin
  CheckRealChannel('SetCoupling');
  FDeviceCommunicator.Send(FName+':DISPLAY '+Select(ADisplay,'ON','OFF'));
End;

(**
 * Query current coupling mode
 *
 * [PG] p. 266
 *)
Function TAgilentMSOX3000AChannel.GetCoupling : TCoupling;
Var St : String;
Begin
  CheckRealChannel('GetCoupling');
  St := FDeviceCommunicator.Query(FName+':COUPLING?');
  For Result := Low(TCoupling) to High(TCoupling) do
    if St = CCoupling[Result] then
      Exit;
End;

(**
 * Query current coupling mode
 *
 * [PG] p. 266
 *)
Procedure TAgilentMSOX3000AChannel.SetCoupling(ACoupling : TCoupling);
Begin
  CheckRealChannel('SetCoupling');
  FDeviceCommunicator.Send(FName+':COUPLING '+CCoupling[ACoupling]);
End;

(**
 * Query the vertical sensitivity
 *
 * [PG] p. 279
 *)
Function TAgilentMSOX3000AChannel.GetVDiv : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(FName+':SCALE?'));
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
 * [PG] p. 279
 *)
Procedure TAgilentMSOX3000AChannel.SetVDiv(AVDiv : Double);
Begin
  FDeviceCommunicator.Send(FName+':SCALE '+FloatToStrF(AVDiv,ffExponent,2,2));
End;

(**
 * Set the vertical position (offset) of the channel
 *
 * AOffset is interpreted in volts.
 *
 * [PG] p. 271
 *)
Procedure TAgilentMSOX3000AChannel.SetOffset(AOffset : Double);
Begin
  FDeviceCommunicator.Send(FName+':OFFSET '+FloatToStrF(AOffset,ffExponent,4,2));
End;

(**
 * Query the vertical offset
 *
 * [PG] p. 271
 *)
Function TAgilentMSOX3000AChannel.GetOffset:Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(FName+':OFFSET?'));
End;

Procedure TAgilentMSOX3000AChannel.CheckRealChannel(AMethod:String);
Begin
  if not (FChannel in CAgilentMSOX3000AChannelReal) then
    Exception.Create(AMethod+' is only allowed for real channels');
End;

End.

