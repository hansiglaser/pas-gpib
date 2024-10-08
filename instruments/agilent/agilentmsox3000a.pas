(**
 * Remote control of Agilent MSO-X 3000A oscilloscopes
 *
 * [PG] Keysight InfiniiVision 3000 X-Series Oscilloscopes Programmer's Guide
 *      Version 02.38.0000, June 9, 2015
 *)
Unit AgilentMSOX3000A;

{$mode objfpc}{$H+}
{$MODESWITCH NestedProcVars}

Interface

Uses
  Classes, SysUtils, TypInfo, Math,
  PasGpibUtils, AsciiDiagram,
  DevCom, RemoteInstrument;

Type
  TTimebaseMode  = (tmMain, tmWindow, tmXY, tmRoll);
  TCoupling      = (cpAC,cpDC);
  TTriggerSource = (tsCH1,tsCH2,tsCH3,tsCH4,
                    tsDig0,tsDig1,tsDig2,tsDig3,tsDig4,tsDig5,tsDig6,tsDig7,tsDig8,tsDig9,tsDig10,tsDig11,tsDig12,tsDig13,tsDig14,tsDig15,
                    tsEXT,tsLine,tsWGen);
  TTriggerType   = (ttEdge,ttGlitch,ttPattern,ttTV,ttDelay,ttEBurst,ttOr,ttRunt,ttShold,ttTransition,ttSBus1,ttSBus2,ttUSB);
  TTriggerSlope  = (tsNegative, tsPositive, tsEither, tsAlternate);
  TTriggerMode   = (tmAuto,tmNormal);
  TImageFormat   = (ifBMP, ifBMP8bit, ifPNG);
  TImagePalette  = (ipColor, ipGrayscale);
  TMeasureType   = (mtVPP, mtVMax, mtVMin,
                    mtVAmplitude, mtVTop, mtVBase,
                    mtOvershoot, mtPreshoot,
                    mtVAverage {N cycles or Display},
                    mtVRMS {N cycles or Display, AC/DC}, mtVRatio {N cycles or Display, Source2},
                    mtPeriod, mtFrequency, mtCounter, mtPWidth, mtNWidth, {TODO: bitrate} mtBurstWidth, mtDutyCycle, {TODO: neg. duty cycle?}
                    mtRisetime, mtFalltime,
                    mtDelay {Source2}, mtPhase {Source2},
                    mtXatMaxY, mtXatMinY,
                    mtPPulseCount, mtNPulseCount,
                    mtPEdgeCount, mtNEdgeCount,
                    mtArea {N cycles or Display});
                    // VTIME, ...
  TMeasureSource = (msCH1,msCH2,msCH3,msCH4,
                    msDig0,msDig1,msDig2,msDig3,msDig4,msDig5,msDig6,msDig7,msDig8,msDig9,msDig10,msDig11,msDig12,msDig13,msDig14,msDig15,
                    msFunction,msMath,msWMemory1,msWMemory2);
  TStatisticsType= (stAll,stCurrent,stMinimum,stMaximum,stMean,stStdDev,stCount);
  TAcquireMode        = (amRealtime, amSegmented);
  TAcquireType        = (atNormal, atPeak, atAverage, atHighRes);
  TSerialMode         = (smNone, smA429,smFlexRay,smCAN,smI2S,smI2C,smLIN,smM1553,smSPI,smUART);
  TWaveformSource     = (wsCH1,wsCH2,wsCH3,wsCH4,
                         wsPOD1,wsPOD2,wsBus1,wsBus2,
                         wsFunction,wsMath,wsWMemory1,wsWMemory2,wsSBus1,wsSBus2);
  TWaveformSubSource  = (wsSub0,wsSub1);
  TWaveformFormat     = (wfByte, wfWord, wfAscii);
  TWaveformPointsMode = (wpmNormal, wpmMaximum, wpmRaw);
  TWaveformByteOrder  = (wboLSBFirst,wboMSBFirst);

Const
  // use the abbreviated forms because a quera only returns that, then the search in this array is simpler
  CTimebaseMode  : Array[TTimebaseMode]  of String = ('MAIN','WIND','XY','ROLL');
  CCoupling      : Array[TCoupling]      of String = ('AC','DC');
  CTriggerSource : Array[TTriggerSource] of String = ('CHAN1','CHAN2','CHAN3','CHAN4',
                                                      'DIG0','DIG1','DIG2','DIG3','DIG4','DIG5','DIG6','DIG7','DIG8','DIG9','DIG10','DIG11','DIG12','DIG13','DIG14','DIG15',
                                                      'EXT','LINE','WGEN');
  CTriggerType   : Array[TTriggerType]   of String = ('EDGE','GLIT','PAT','TV','DEL','EBUR','OR','RUNT','SHOL','TRAN','SBUS1','SBUS2','USB');
  CTriggerSlope  : Array[TTriggerSlope]  of String = ('NEG', 'POS', 'EITH', 'ALT');
  CTriggerMode   : Array[TTriggerMode]   of String = ('AUTO','NORMAL');
  CImageFormat   : Array[TImageFormat]   of String = ('BMP','BMP8','PNG');
  CImagePalette  : Array[TImagePalette]  of String = ('COL','GRAY');
  CMeasureType   : Array[TMeasureType]   of String = ('VPP','VMAX','VMIN',
                                                      'VAMP','VTOP','VBAS',
                                                      'OVER','PRES',
                                                      'VAV',
                                                      'VRMS','VRAT',
                                                      'PER','FREQ','COUN','PWID','NWID','BWID','DUTY',
                                                      'RIS','FALL',
                                                      'DEL','PHAS',
                                                      'XMAX','YMAX',
                                                      'PPUL','NPUL',
                                                      'PEDG','NEDG',
                                                      'ARE');
  CMeasureTypeCycles:Array[TMeasureType] of Boolean= (False, False, False,
                                                      False, False, False,
                                                      False, False,
                                                      True,
                                                      True, True,
                                                      False, False, False, False, False, False, False,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      True);
  CMeasureTypeACDC : Array[TMeasureType] of Boolean= (False, False, False,
                                                      False, False, False,
                                                      False, False,
                                                      False,
                                                      True, False,
                                                      False, False, False, False, False, False, False,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      False);
  CMeasureTypeSrc2 : Array[TMeasureType] of Boolean= (False, False, False,
                                                      False, False, False,
                                                      False, False,
                                                      False,
                                                      False, True,
                                                      False, False, False, False, False, False, False,
                                                      False, False,
                                                      True, True,
                                                      False, False,
                                                      False, False,
                                                      False, False,
                                                      False);
  CMeasureSource : Array[TMeasureSource] of String = ('CHAN1','CHAN2','CHAN3','CHAN4',
                                                      'DIG0','DIG1','DIG2','DIG3','DIG4','DIG5','DIG6','DIG7','DIG8','DIG9','DIG10','DIG11','DIG12','DIG13','DIG14','DIG15',
                                                      'FUNC','MATH','WMEM1','WMEM2');
  CStatisticsType: Array[TStatisticsType] of String= ('ON','CURR','MIN','MAX','MEAN','STDD','COUN');
  COpStatRun           = 1 shl  3;   // [PG] p. 207
  COpStatWaitTrig      = 1 shl  5;
  COpStatPowerEvent    = 1 shl  7;
  COpStatMaskTestEvent = 1 shl  9;
  COpStatOverload      = 1 shl 11;
  CAcquireMode        : Array[TAcquireMode]        of String = ('RTIM', 'SEGM');
  CAcquireType        : Array[TAcquireType]        of String = ('NORM', 'PEAK', 'AVER','HRES');
  CSerialMode         : Array[TSerialMode]         of String = ('NONE', 'A429', 'FLEX', 'CAN', 'I2S', 'IIC', 'LIN', 'M1553', 'SPI', 'UART');
  CWaveformSource     : Array[TWaveformSource]     of String = ('CHAN1','CHAN2','CHAN3','CHAN4',
                                                                'POD1','POD2','BUS1','BUS2',
                                                                'FUNC','MATH','WMEM1','WMEM2','SBUS1','SBUS2');
  CWaveformSubSource  : Array[TWaveformSubSource]  of String = ('SUB0','SUB1');
  CWaveformFormat     : Array[TWaveformFormat]     of String = ('BYTE', 'WORD', 'ASC');
  CWaveformPointsMode : Array[TWaveformPointsMode] of String = ('NORM', 'MAX', 'RAW');
  CWaveformByteOrder  : Array[TWaveformByteOrder]  of String = ('LSBF','MSBF');

Type
  TAgilentMSOX3000AChannel = class;
  TAgilentMSOX3000AChannelIndex = (CH1,CH2,CH3,CH4);
  TAgilentMSOX3000AChannelSet = set of TAgilentMSOX3000AChannelIndex;

Const
   CAgilentMSOX3000AChannelName : Array[TAgilentMSOX3000AChannelIndex] of String = ('CHAN1','CHAN2','CHAN3','CHAN4');
   CAgilentMSOX3000AChannelReal = [CH1,CH2,CH3,CH4];

Type
  TMeasureResult = record
    Name    : String;
    Current : Double;
    Min     : Double;
    Max     : Double;
    Mean    : Double;
    StdDev  : Double;
    Count   : Int64;
  End;
  TDynMeasureResultArray = Array of TMeasureResult;

  TEventData = record
    Timestamp : Double;
    Event     : String;
  End;
  TDynEventArray = Array of TEventData;

  { TWaveform }

  TWaveform = class
    FSource     : TWaveformSource;
    FSubSource  : TWaveformSubSource;
    FFormat     : TWaveformFormat;
    FType       : TAcquireType;
    FPoints     : Integer;
    FAvgCount   : Integer;
    FXIncrement : Double;
    FXOrigin    : Double;
    FXReference : Integer;
    FYIncrement : Double;
    FYOrigin    : Double;
    FYReference : Integer;
    FPointsMode : TWaveformPointsMode;
    FByteOrder  : TWaveformByteOrder;
    FUnsigned   : Boolean;
    FByteData   : TDynByteArray;
    FWordData   : TDynWordArray;
    FRealData   : TDynDoubleArray;
    FSerialData : TDynEventArray;
    FTimes      : TDynDoubleArray;
    Constructor CreateFromCSV(AFilename:String);
    Procedure PrintPreamble;
    Procedure ConvToReal;
    Function  GetTime(Index:Integer) : Double;
    Procedure ConvTimes;
    Procedure Merge(AWaveform:TWaveform);
    Procedure PrintAsciiArt(Width, Height : Integer; XDiv, YDiv : Double);
    Procedure PrintSerialData;
    Procedure SaveSerialData(Filename:String);
    Procedure SaveCSV(AFilename:String);
    Procedure LoadCSV(AFilename:String);
  End;

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
    Procedure TriggerForce;
    // Acquire
    Procedure SetAcquireMode(AMode:TAcquireMode);
    Function  GetAcquireMode : TAcquireMode;
    Procedure SetAcquireType(AType:TAcquireType);
    Function  GetAcquireType : TAcquireType;
    Procedure SetAcquireCount(ACount:Integer);
    Function  GetAcquireCount : Integer;
    Function  GetAcquirePoints : Integer;
    Procedure SetAcquireSegCount(ACount:Integer);
    Function  GetAcquireSegCount : Integer;
    Function  GetAcquireSampleRate : Double;
    // Status
    Function  GetOperationStatusCondition : Word;
    // Cursor
    // Serial Decode
    Class Function  WaveformSource2SerialBusNum(ASource : TWaveformSource) : Integer;
    Function  GetSerialMode(ABusNum:Integer) : TSerialMode;
    // Hard Copy
    Procedure SetHardcopyOptions(AInkSaver : Boolean);
    Function  GetHardcopyOptions : String;
    // Data transfer
    Function Screen(AImageFormat : TImageFormat; AImagePalette : TImagePalette) : TDynByteArray;
    // Waveform
    Procedure SetWaveformSource(ASource:TWaveformSource);
    Function  GetWaveformSource : TWaveformSource;
    Procedure SetWaveformSubSource(ASubSource:TWaveformSubSource);
    Function  GetWaveformSubSource : TWaveformSubSource;
    Procedure SetWaveformFormat(AFormat:TWaveformFormat);
    Function  GetWaveformFormat : TWaveformFormat;
    Procedure SetWaveformPointsCount(ACount:Integer);
    Function  GetWaveformPointsCount:Integer;
    Procedure SetWaveformPointsMode(APointsMode:TWaveformPointsMode);
    Function  GetWaveformPointsMode : TWaveformPointsMode;
    Procedure SetWaveformByteOrder(AByteOrder:TWaveformByteOrder);
    Function  GetWaveformByteOrder : TWaveformByteOrder;
    Procedure SetWaveformUnsigned(AUnsigned:Boolean);
    Function  GetWaveformUnsigned : Boolean;
    Function  GetWaveformPreamble : TWaveform;
    Procedure GetWaveformData(AWaveform : TWaveform);
    Function  GetWaveformData : TWaveform;
    // Math
    // Display
    Function  GetTimebaseMode : TTimebaseMode;
    Procedure SetTimebaseMode(ATimebaseMode : TTimebaseMode);
    // IEEE 488.1 Emulation
    // Others
    // Measure
    Procedure MeasureAdd(AMeasureType:TMeasureType;ASource:TMeasureSource);
    Procedure MeasureAdd(AMeasureType:TMeasureType;ASource:TMeasureSource;ACyclesDisplay:Integer);
    Procedure MeasureAdd(AMeasureType:TMeasureType;ASource:TMeasureSource;ACyclesDisplay:Integer;AACDC:Boolean);
    Procedure MeasureAdd(AMeasureType:TMeasureType;ASource:TMeasureSource;ACyclesDisplay:Integer;ASource2:TMeasureSource);
    Procedure MeasureAdd(AMeasureType:TMeasureType;ASource:TMeasureSource;ASource2:TMeasureSource);
    Procedure MeasureClear;
    Function  GetMeasureResults : TDynMeasureResultArray;
    Function  GetMeasureResults(AStatisticsType:TStatisticsType) : TDynDoubleArray;
    Procedure SetMeasureStatistics(AStatisticsType:TStatisticsType);
    Function  GetMeasureStatistics : TStatisticsType;
    Procedure MeasureStatisticsReset;
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
    Procedure SetBWLimit(ALimit:Boolean);
    Function  GetBWLimit : Boolean;
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
 * Acquire a screenshot and return its data
 *
 * Use the method HardcopySet to set inverted screen, ...
 *
 * [PG] p. 306
 *)
Function TAgilentMSOX3000A.Screen(AImageFormat:TImageFormat;AImagePalette:TImagePalette) : TDynByteArray;
Begin
  Result := QueryBinary(':DISPLAY:DATA? '+CImageFormat[AImageFormat]+', '+CImagePalette[AImagePalette]);
End;

(**
 * Select source for waveform commands
 *
 * [PG] p. 979ff
 *)
Procedure TAgilentMSOX3000A.SetWaveformSource(ASource : TWaveformSource);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:SOURCE '+CWaveformSource[ASource])
End;

(**
 * Query source for waveform commands
 *
 * [PG] p. 979ff
 *)
Function TAgilentMSOX3000A.GetWaveformSource : TWaveformSource;
Begin
  Result := TWaveformSource(Find(FDeviceCommunicator.Query(':WAVEFORM:SOURCE?'), CWaveformSource, 'Invalid waveform source'));
End;

(**
 * Select sub-source for waveform commands for the source SBUS<n>
 *
 * [PG] p. 983ff
 *)
Procedure TAgilentMSOX3000A.SetWaveformSubSource(ASubSource : TWaveformSubSource);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:SOURCE:SUBSOURCE '+CWaveformSubSource[ASubSource])
End;

(**
 * Query sub-source for waveform commands for the source SBUS<n>
 *
 * [PG] p. 983ff
 *)
Function TAgilentMSOX3000A.GetWaveformSubSource : TWaveformSubSource;
Begin
  Result := TWaveformSubSource(Find(FDeviceCommunicator.Query(':WAVEFORM:SOURCE:SUBSOURCE?'), CWaveformSubSource, 'Invalid waveform subsource'));
End;

(**
 * Select waveform transmission format
 *
 * Byte vs. Word doesn't provide higher resolution (i.e., less quantization),
 * even when only retrieving a smaller sample count, neither for normal nor for
 * raw data format.
 *
 * [PG] p. 969
 *)
Procedure TAgilentMSOX3000A.SetWaveformFormat(AFormat : TWaveformFormat);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:FORMAT '+CWaveformFormat[AFormat])
End;

(**
 * Query waveform transmission format
 *
 * [PG] p. 969
 *)
Function TAgilentMSOX3000A.GetWaveformFormat : TWaveformFormat;
Begin
  Result := TWaveformFormat(Find(FDeviceCommunicator.Query(':WAVEFORM:FORMAT?'), CWaveformFormat, 'Invalid waveform format'));
End;

(**
 * Set number of points to be retrieved for a waveform
 *
 * Specify a value <=0 to request the maximum number of points.
 *
 * [PG] p. 970f
 *)
Procedure TAgilentMSOX3000A.SetWaveformPointsCount(ACount : Integer);
Begin
  if ACount > 0 then
    FDeviceCommunicator.Send(':WAVEFORM:POINTS '+IntToStr(ACount))
  else
    FDeviceCommunicator.Send(':WAVEFORM:POINTS MAX')
End;

(**
 * Query number of points to be retrieved for a waveform
 *
 * [PG] p. 970f
 *)
Function TAgilentMSOX3000A.GetWaveformPointsCount : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query(':WAVEFORM:POINTS?'));
End;

(**
 * Set waveform points mode
 *
 * [PG] p. 972f
 *)
Procedure TAgilentMSOX3000A.SetWaveformPointsMode(APointsMode : TWaveformPointsMode);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:POINTS:MODE '+CWaveformPointsMode[APointsMode]);
End;

(**
 * Query waveform points mode
 *
 * [PG] p. 972f
 *)
Function TAgilentMSOX3000A.GetWaveformPointsMode : TWaveformPointsMode;
Begin
  Result := TWaveformPointsMode(Find(FDeviceCommunicator.Query(':WAVEFORM:POINTS:MODE?'), CWaveformPointsMode, 'Invalid waveform points mode'));
End;

(**
 * Set waveform byte order
 *
 * [PG] p. 965
 *)
Procedure TAgilentMSOX3000A.SetWaveformByteOrder(AByteOrder : TWaveformByteOrder);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:BYTEORDER '+CWaveformByteOrder[AByteOrder]);
End;

(**
 * Query waveform byte order
 *
 * [PG] p. 965
 *)
Function TAgilentMSOX3000A.GetWaveformByteOrder : TWaveformByteOrder;
Begin
  Result := TWaveformByteOrder(Find(FDeviceCommunicator.Query(':WAVEFORM:BYTEORDER?'), CWaveformByteOrder, 'Invalid waveform byte order'));
End;

(**
 * Set waveform unsigned transfer setting
 *
 * [PG] p. 985
 *)
Procedure TAgilentMSOX3000A.SetWaveformUnsigned(AUnsigned : Boolean);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:POINTS:MODE '+Select(AUnsigned,'ON','OFF'));
End;

(**
 * Query waveform unsigned transfer setting
 *
 * [PG] p. 985
 *)
Function TAgilentMSOX3000A.GetWaveformUnsigned : Boolean;
Begin
  Result := (FDeviceCommunicator.Query(':WAVEFORM:FORMAT?') = '1');
End;

(**
 * Query waveform preamble information
 *
 * [PG] p. 374ff
 *)
Function TAgilentMSOX3000A.GetWaveformPreamble : TWaveform;
Var St    : String;
    StArr : TDynStringArray;
Begin
  St := FDeviceCommunicator.Query(':WAVEFORM:PREAMBLE?');
  StArr := SplitStr(',', St);
  if Length(StArr) <> 10 then
    raise Exception.Create('Ill formatted waveform preamble info '''+St+'''');
  Result := TWaveform.Create;
  // format
  Case StArr[0] of
    '+0' : Result.FFormat := wfByte;
    '+1' : Result.FFormat := wfWord;
    '+4' : Result.FFormat := wfAscii;
  else
    raise Exception.Create('Invalid waveform preamble format '''+StArr[0]+'''');
  End;
  // type
  Case StArr[1] of
    '+0' : Result.FType := atNormal;
    '+1' : Result.FType := atPeak;
    '+2' : Result.FType := atAverage;
    '+3' : Result.FType := atHighRes;
  else
    raise Exception.Create('Invalid waveform preamble type '''+StArr[0]+'''');
  End;
  // and simpler values
  Result.FPoints     := StrToInt  (StArr[2]);
  Result.FAvgCount   := StrToInt  (StArr[3]);
  Result.FXIncrement := StrToFloat(StArr[4]);
  Result.FXOrigin    := StrToFloat(StArr[5]);
  Result.FXReference := StrToInt  (StArr[6]);
  Result.FYIncrement := StrToFloat(StArr[7]);
  Result.FYOrigin    := StrToFloat(StArr[8]);
  Result.FYReference := StrToInt  (StArr[9]);
  // and more information
  Result.FSource     := GetWaveformSource;
  Result.FSubSource  := GetWaveformSubSource;
  Result.FPointsMode := GetWaveformPointsMode;
  Result.FByteOrder  := GetWaveformByteOrder;
  Result.FUnsigned   := GetWaveformUnsigned;
End;

Procedure TAgilentMSOX3000A.GetWaveformData(AWaveform : TWaveform);
Var St : String;
    SA : TDynStringArray;
    I  : Integer;
Begin
  AWaveform.FByteData := QueryBinary(':WAVEFORM:DATA?');
  if AWaveform.FFormat = wfWord then
    Begin
      // copy to FWordData
      SetLength(AWaveform.FWordData, Length(AWaveform.FByteData) shr 1);
      Move(AWaveform.FByteData[0], AWaveform.FWordData[0], Length(AWaveform.FByteData));
      SetLength(AWaveform.FByteData, 0);
      if AWaveform.FByteOrder = wboMSBFirst then
        for I := 0 to Length(AWaveform.FWordData)-1 do
          AWaveform.FWordData[I] := Swap(AWaveform.FWordData[I]);
    End
  else if AWaveform.FFormat = wfAscii then
    Begin
      // copy to String
      SetLength(St, Length(AWaveform.FByteData));
      Move(AWaveform.FByteData[0], St[1], Length(AWaveform.FByteData));
      SetLength(AWaveform.FByteData, 0);
      if not (AWaveform.FSource in [wsSBus1, wsSBus2]) then
        Begin
          // convert numeric data
          // split and convert to double
          AWaveform.FRealData := SplitDouble(',', St);
        End
      else
        Begin
          // convert serial decode bus data
          // the returned string is '<time0>,<data0>,<time1>,<data1>,...', and it has many spaces
          // split
          //SA := SplitStr(',', St);
          SA := St.Split([',']);;
          if Length(SA) <> AWaveform.FPoints*2 then
            Begin
              WriteLn('Length(SA) = ',Length(SA),' <> AWaveform.Points*2 = ',AWaveform.FPoints*2);
              Exit;
              // TODO: Exception
            End;
          // convert
          SetLength(AWaveform.FSerialData, AWaveform.FPoints);
          For I := 0 to AWaveform.FPoints-1 do
            With AWaveform.FSerialData[I] do
              Begin
                Timestamp := StrToFloat(Trim(SA[I*2]));
                Event     := Trim(SA[I*2+1]);
              End;
        End;
    End;
End;

Function TAgilentMSOX3000A.GetWaveformData : TWaveform;
Begin
  Result := GetWaveformPreamble;
  GetWaveformData(Result);
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
 * Force the trigger now
 *
 * [PG] p. 878
 *)
Procedure TAgilentMSOX3000A.TriggerForce;
Begin
  FDeviceCommunicator.Send(':TRIGGER:FORCE');
End;

(**
 * Set the acquisition mode (realtime vs. segmented)
 *
 * [PG] p. 231
 *)
Procedure TAgilentMSOX3000A.SetAcquireMode(AMode : TAcquireMode);
Begin
  FDeviceCommunicator.Send(':ACQUIRE:MODE '+CAcquireMode[AMode]);
End;

(**
 * Query the acquisition mode (realtime vs. segmented)
 *
 * [PG] p. 231
 *)
Function TAgilentMSOX3000A.GetAcquireMode : TAcquireMode;
Var St : String;
Begin
  St := FDeviceCommunicator.Query(':ACQUIRE:MODE?');
  For Result := Low(TAcquireMode) to High(TAcquireMode) do
    if St = CAcquireMode[Result] then
      Exit;
  raise Exception.Create('Unknown acquire mode '''+St+'''');
End;

(**
 * Set the acqiwtion type (normal, average, high resolution, peak)
 *
 * [PG] p. 239
 *)
Procedure TAgilentMSOX3000A.SetAcquireType(AType : TAcquireType);
Begin
  FDeviceCommunicator.Send(':ACQUIRE:TYPE '+CAcquireType[AType]);
End;

(**
 * Query the acqiwtion type (normal, average, high resolution, peak)
 *
 * [PG] p.
 *)
Function TAgilentMSOX3000A.GetAcquireType : TAcquireType;
Var St : String;
Begin
  St := FDeviceCommunicator.Query(':ACQUIRE:TYPE?');
  For Result := Low(TAcquireType) to High(TAcquireType) do
    if St = CAcquireType[Result] then
      Exit;
  raise Exception.Create('Unknown acquire type '''+St+'''');
End;

(**
 * Set the number of values to be averaged
 *
 * Set the number of values to be averaged for each time bucket before the
 * acquisition is considered to be complete for that time bucket. When
 * :ACQuire:Mode is set to AVERage, the count can be set to any value from
 * 2 to 65536.
 *
 * [PG] p. 230
 *)
Procedure TAgilentMSOX3000A.SetAcquireCount(ACount : Integer);
Begin
  FDeviceCommunicator.Send(':ACQUIRE:COUNT '+IntToStr(ACount));
End;

(**
 * Query the currently selected count value for averaging mode
 *
 * [PG] p. 230
 *)
Function TAgilentMSOX3000A.GetAcquireCount : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query(':ACQUIRE:COUNT?'));
End;

(**
 * Query the number of data points that the hardware will acquire from the input signal
 *
 * The number of points acquired is not directly controllable.
 *
 * [PG] p. 232
 *)
Function TAgilentMSOX3000A.GetAcquirePoints : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query(':ACQUIRE:POINTS?'));
End;

(**
 * Set the number of memory segments to acquire
 *
 * [PG] p. 234
 *)
Procedure TAgilentMSOX3000A.SetAcquireSegCount(ACount : Integer);
Begin
  FDeviceCommunicator.Send(':ACQUIRE:SEGMENTED:COUNT '+IntToStr(ACount));
End;

(**
 * Query the number of memory segments to acquire
 *
 * [PG] p. 234
 *)
Function TAgilentMSOX3000A.GetAcquireSegCount : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query(':ACQUIRE:SEGMENTED:COUNT?'));
End;

(**
 * Query the current oscilloscope acquisition sample rate
 *
 * The sample rate is not directly controllable.
 *
 * [PG] p. 238
 *)
Function TAgilentMSOX3000A.GetAcquireSampleRate : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(':ACQUIRE:SRATE?'));
End;

(**
 * Query Operation Status Condition Register
 *
 * Use the constants COpStat*
 *
 * [PG] p. 207
 *)
Function TAgilentMSOX3000A.GetOperationStatusCondition : Word;
Begin
  Result := StrToInt(FDeviceCommunicator.Query(':OPEREGISTER:CONDITION?'));
End;

(**
 * Convert a TWaveformSource to a serial bus number
 *)
Class Function TAgilentMSOX3000A.WaveformSource2SerialBusNum(ASource : TWaveformSource) : Integer;
Begin
  if      ASource = wsSBus1 then Result := 1
  else if ASource = wsSBus2 then Result := 2
  else
    raise Exception.Create('Waveform source '+CWaveformSource[ASource]+' is not a serial waveform.');
End;

(**
 * Query decode mode of a serial bus
 *
 * ABusNum is 1 or 2
 *
 * [PG] p. 633
 *)
Function TAgilentMSOX3000A.GetSerialMode(ABusNum : Integer) : TSerialMode;
Var St : String;
Begin
  St := FDeviceCommunicator.Query(':SBUS'+IntToStr(ABusNum)+':MODE?');
  For Result := Low(TSerialMode) to High(TSerialMode) do
    if St = CSerialMode[Result] then
      Exit;
  raise Exception.Create('Unknown serial decode mode  '''+St+'''');
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
  raise Exception.Create('Unknown timebase mode '''+St+'''');
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
 * Add a measurement to the display
 *
 * This function can be used for measurement types without parameters (except
 * the source channel).
 *
 * [PG] p. 395ff
 *)
Procedure TAgilentMSOX3000A.MeasureAdd(AMeasureType : TMeasureType; ASource : TMeasureSource);
Begin
  if CMeasureTypeCycles[AMeasureType] or CMeasureTypeACDC[AMeasureType] or CMeasureTypeSrc2[AMeasureType] then
    raise Exception.Create('Wrong overloaded function used for MeasureType '+GetEnumName(TypeInfo(TMeasureType), Ord(AMeasureType)));
  FDeviceCommunicator.Send(':MEASURE:'+CMeasureType[AMeasureType]+' '+CMeasureSource[ASource]);
End;

(**
 * Add a measurement to the display
 *
 * This function can be used for measurement types with an interval (0: full
 * screen, >0: number of cycles).
 *
 * [PG] p. 395ff
 *)
Procedure TAgilentMSOX3000A.MeasureAdd(AMeasureType : TMeasureType; ASource : TMeasureSource; ACyclesDisplay : Integer);
Begin
  if (not CMeasureTypeCycles[AMeasureType]) or CMeasureTypeACDC[AMeasureType] or CMeasureTypeSrc2[AMeasureType] then
    raise Exception.Create('Wrong overloaded function used for MeasureType '+GetEnumName(TypeInfo(TMeasureType), Ord(AMeasureType)));
  FDeviceCommunicator.Send(':MEASURE:'+CMeasureType[AMeasureType]+' '
    +Select(ACyclesDisplay<=0,'DISP',IntToStr(ACyclesDisplay))
    +','+CMeasureSource[ASource]);
End;

(**
 * Add a measurement to the display
 *
 * This function can be used for measurement types with an interval (0: full
 * screen, >0: number of cycles) and coupling type (true: AC, false: DC).
 *
 * [PG] p. 395ff
 *)
Procedure TAgilentMSOX3000A.MeasureAdd(AMeasureType : TMeasureType; ASource : TMeasureSource; ACyclesDisplay : Integer; AACDC : Boolean);
Begin
  if (not CMeasureTypeCycles[AMeasureType]) or (not CMeasureTypeACDC[AMeasureType]) or CMeasureTypeSrc2[AMeasureType] then
    raise Exception.Create('Wrong overloaded function used for MeasureType '+GetEnumName(TypeInfo(TMeasureType), Ord(AMeasureType)));
  FDeviceCommunicator.Send(':MEASURE:'+CMeasureType[AMeasureType]+' '
    +Select(ACyclesDisplay<=0,'DISP',IntToStr(ACyclesDisplay))
    +','+Select(AACDC, 'AC', 'DC')
    +','+CMeasureSource[ASource]);
End;

(**
 * Add a measurement to the display
 *
 * This function can be used for measurement types with an interval (0: full
 * screen, >0: number of cycles) and a second source channel.
 *
 * [PG] p. 395ff
 *)
Procedure TAgilentMSOX3000A.MeasureAdd(AMeasureType : TMeasureType; ASource : TMeasureSource; ACyclesDisplay : Integer; ASource2 : TMeasureSource);
Begin
  if (not CMeasureTypeCycles[AMeasureType]) or CMeasureTypeACDC[AMeasureType] or (not CMeasureTypeSrc2[AMeasureType]) then
    raise Exception.Create('Wrong overloaded function used for MeasureType '+GetEnumName(TypeInfo(TMeasureType), Ord(AMeasureType)));
  FDeviceCommunicator.Send(':MEASURE:'+CMeasureType[AMeasureType]+' '
    +Select(ACyclesDisplay<=0,'DISP',IntToStr(ACyclesDisplay))
    +','+CMeasureSource[ASource]
    +','+CMeasureSource[ASource2]);
End;

(**
 * Add a measurement to the display
 *
 * This function can be used for measurement types with a second source channel.
 *
 * [PG] p. 395ff
 *)
Procedure TAgilentMSOX3000A.MeasureAdd(AMeasureType : TMeasureType; ASource : TMeasureSource; ASource2 : TMeasureSource);
Begin
  if CMeasureTypeCycles[AMeasureType] or CMeasureTypeACDC[AMeasureType] or (not CMeasureTypeSrc2[AMeasureType]) then
    raise Exception.Create('Wrong overloaded function used for MeasureType '+GetEnumName(TypeInfo(TMeasureType), Ord(AMeasureType)));
  FDeviceCommunicator.Send(':MEASURE:'+CMeasureType[AMeasureType]+' '
    +CMeasureSource[ASource]
    +','+CMeasureSource[ASource2]);
End;

(**
 * Query the results of the measurements
 *
 * This function is only allowed when SetMeasureStatistics(stAll) was used
 * before.
 *
 * [PG] p. 438ff
 *)
Function TAgilentMSOX3000A.GetMeasureResults : TDynMeasureResultArray;
Var St : String;
    A  : TDynStringArray;
    I  : Integer;
Begin
  SetLength(Result, 0);
  St := FDeviceCommunicator.Query(':MEASURE:RESULTS?');
  if St = '' then   // no measurements were setup
    Exit;
  A := SplitStr(',', St);
  if (Length(A) mod 7) <> 0 then
    raise Exception.Create('GetMeasureResults: Invalid string received: '''+St+'''. Did you SetMeasureStatistics(stAll)?');
  SetLength(Result, Length(A) div 7);
  For I := 0 to Length(Result)-1 do
    Begin
      Result[I].Name    :=            A[I*7+0];
      Result[I].Current := StrToFloat(A[I*7+1]);
      Result[I].Min     := StrToFloat(A[I*7+2]);
      Result[I].Max     := StrToFloat(A[I*7+3]);
      Result[I].Mean    := StrToFloat(A[I*7+4]);
      Result[I].StdDev  := StrToFloat(A[I*7+5]);
      Result[I].Count   := StrToInt64(A[I*7+6]);
    End;
End;

(**
 * Query the results of the measurements
 *
 * This function is only allowed when SetMeasureStatistics with a parameter
 * other than stAll was used before.
 *
 * The parameter AStatisticsType is unused, it is only there to overload the
 * function.
 *
 * [PG] p. 438ff
 *)
Function TAgilentMSOX3000A.GetMeasureResults(AStatisticsType : TStatisticsType) : TDynDoubleArray;
Var St : String;
    A  : TDynStringArray;
    I  : Integer;
Begin
  St := FDeviceCommunicator.Query(':MEASURE:RESULTS?');
  // The string is just a comma-separated list with values, one for each setup
  // measurement, which shows the statistics value selected with
  // SetMeasureStatistics
  if St = '' then   // no measurements were setup
    Exit;
  A := SplitStr(',', St);
  if Length(A) > 4 then
    raise Exception.Create('GetMeasureResults: Invalid string received: '''+St+'''. Did you SetMeasureStatistics(stAll)?');
  SetLength(Result, Length(A));
  For I := 0 to Length(A)-1 do
    Result[I] := StrToFloat(A[I]);
End;

(**
 * Clear all selected measurements and markers from the screen
 *
 * [PG] p. 411
 *)
Procedure TAgilentMSOX3000A.MeasureClear;
Begin
  FDeviceCommunicator.Send(':MEASURE:CLEAR');
End;

(**
 * Select the statistics type which can be queried with GetMeasureResults
 *
 * [PG] p. 446
 *)
Procedure TAgilentMSOX3000A.SetMeasureStatistics(AStatisticsType : TStatisticsType);
Begin
  FDeviceCommunicator.Send(':MEASURE:STATISTICS '+CStatisticsType[AStatisticsType]);
End;

(**
 * Query the statistics type which can be queried with GetMeasureResults
 *
 * [PG] p. 446
 *)
Function TAgilentMSOX3000A.GetMeasureStatistics : TStatisticsType;
Var St : String;
Begin
  St := FDeviceCommunicator.Query(':MEASURE:STATISTICS?');
  For Result := Low(TStatisticsType) to High(TStatisticsType) do
    if St = CStatisticsType[Result] then
      Exit;
  raise Exception.Create('Unknown statistics type '''+St+'''');
End;

(**
 * Reset the measurement statistics and zero the counts
 *
 * The measurement (statistics) configuration is not deleted.
 *
 * [PG] p. 450
 *)
Procedure TAgilentMSOX3000A.MeasureStatisticsReset;
Begin
  FDeviceCommunicator.Send(':MEASURE:STATISTICS:RESET');
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
  FDeviceCommunicator.Send(':'+FName+':DISPLAY '+Select(ADisplay,'ON','OFF'));
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
  St := FDeviceCommunicator.Query(':'+FName+':COUPLING?');
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
  FDeviceCommunicator.Send(':'+FName+':COUPLING '+CCoupling[ACoupling]);
End;

(**
 * Query the vertical sensitivity
 *
 * [PG] p. 279
 *)
Function TAgilentMSOX3000AChannel.GetVDiv : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(':'+FName+':SCALE?'));
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
  FDeviceCommunicator.Send(':'+FName+':SCALE '+FloatToStrF(AVDiv,ffExponent,2,2));
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
  FDeviceCommunicator.Send(':'+FName+':OFFSET '+FloatToStrF(AOffset,ffExponent,4,2));
End;

(**
 * Query the vertical offset
 *
 * [PG] p. 271
 *)
Function TAgilentMSOX3000AChannel.GetOffset:Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(':'+FName+':OFFSET?'));
End;

(**
 * Set bandwidth limit
 *
 * [PG] p. 265
 *)
Procedure TAgilentMSOX3000AChannel.SetBWLimit(ALimit : Boolean);
Begin
  FDeviceCommunicator.Send(':'+FName+':BWLIMIT '+Select(ALimit, 'ON', 'OFF'));
End;

(**
 * Query bandwidth limit
 *
 * [PG] p. 265
 *)
Function TAgilentMSOX3000AChannel.GetBWLimit : Boolean;
Begin
  Result := (FDeviceCommunicator.Query(':'+FName+':BWLIMIT?') = '1');
End;

Procedure TAgilentMSOX3000AChannel.CheckRealChannel(AMethod:String);
Begin
  if not (FChannel in CAgilentMSOX3000AChannelReal) then
    Exception.Create(AMethod+' is only allowed for real channels');
End;

{ TWaveform }

Constructor TWaveform.CreateFromCSV(AFilename : String);
Begin
  inherited Create;
  LoadCSV(AFilename);
End;

Procedure TWaveform.PrintPreamble;
Begin
  WriteLn('Source    : ', CWaveformSource    [FSource    ]);               // TWaveformSource;
  WriteLn('SubSource : ', CWaveformSubSource [FSubSource ]);               // TWaveformSubSource;
  WriteLn('Format    : ', CWaveformFormat    [FFormat    ]);               // TWaveformFormat;
  WriteLn('Type      : ', CAcquireType       [FType      ]);               // TAcquireType;
  WriteLn('Points    : ', FPoints);                                        // Integer;
  WriteLn('AvgCount  : ', FAvgCount);                                      // Integer;
  WriteLn('XIncrement: ', FloatToStrSI(FXIncrement,FormatSettings), 's');  // Double;
  WriteLn('XOrigin   : ', FloatToStrSI(FXOrigin,   FormatSettings), 's');  // Double;
  WriteLn('XReference: ', FXReference);                                    // Integer;
  WriteLn('YIncrement: ', FloatToStrSI(FYIncrement,FormatSettings), 'V');  // Double;
  WriteLn('YOrigin   : ', FloatToStrSI(FYOrigin,   FormatSettings), 'V');  // Double;
  WriteLn('YReference: ', FYReference);                                    // Integer;
  WriteLn('PointsMode: ', CWaveformPointsMode[FPointsMode]);               // TWaveformPointsMode;
  WriteLn('ByteOrder : ', CWaveformByteOrder [FByteOrder]);                // TWaveformByteOrder;
  WriteLn('Unsigned  : ', FUnsigned);                                      // Boolean;
End;

(**
 * Convert raw values to voltages
 *
 * [PG] p. 961f
 *)
Procedure TWaveform.ConvToReal;
Var I : Integer;
Begin
  if Length(FWordData) > 0 then
    Begin
      // word data takes precedence over byte data
      SetLength(FRealData, Length(FWordData));
      For I := 0 to Length(FWordData)-1 do
        FRealData[I] := ((FWordData[I] - FYReference) * FYIncrement) + FYOrigin;
    End
  else if Length(FByteData) > 0 then
    Begin
      SetLength(FRealData, Length(FByteData));
      For I := 0 to Length(FByteData)-1 do
        FRealData[I] := ((FByteData[I] - FYReference) * FYIncrement) + FYOrigin;
    End
  else if (Length(FRealData) > 0) and (FFormat = wfAscii) then
    Exit    // ASCII is directly converted to FRealData
  else if (Length(FByteData) = 0) and (Length(FWordData) = 0) then
    raise Exception.Create('No byte nor word data available');
End;

(**
 * Convert indices to time relative to the trigger instant
 *
 * [PG] p. 961f
 *)
Function TWaveform.GetTime(Index : Integer) : Double;
Begin
  if FType <> atPeak then
    Result := ((Index - FXReference) * FXIncrement) + FXOrigin
  else
    Result := ((Index - FXReference) * 2 * FXIncrement) + FXOrigin;
End;

(**
 * Convert indices to time relative to the trigger instant
 *
 * [PG] p. 961f
 *)
Procedure TWaveform.ConvTimes;
Var I : Integer;
Begin
  SetLength(FTimes, Length(FRealData));
  if FType <> atPeak then
    Begin
      For I := 0 to Length(FRealData)-1 do
        FTimes[I] := ((I - FXReference) * FXIncrement) + FXOrigin
    End
  else
    Begin
      For I := 0 to Length(FRealData)-1 do
        FTimes[I] := ((I - FXReference) * 2 * FXIncrement) + FXOrigin;
    End;
End;

Procedure TWaveform.Merge(AWaveform : TWaveform);
Var I : Integer;
Begin
  if (Length(FSerialData) = 0) or (Length(AWaveform.FSerialData) = 0) then
    raise Exception.Create('Merge only works for serial decode data in both datasets.');
  if Length(FSerialData) <> Length(AWaveform.FSerialData) then
    raise Exception.Create('Merge only works with same length datasets.');
  For I := 0 to Length(FSerialData)-1 do
    With FSerialData[I] do
      Begin
        if not SameValue(Timestamp, AWaveform.FSerialData[I].Timestamp) then
          raise Exception.Create('Data points number '+IntToStr(I)+' have differing timestamps '+FloatToStr(Timestamp*1e6)+' vs. '+FloatToStr(AWaveform.FSerialData[I].Timestamp*1e6));
        Event := Event + ',' + AWaveform.FSerialData[I].Event;
      End;
End;

Procedure TWaveform.PrintAsciiArt(Width, Height : Integer; XDiv,YDiv:Double);
Var AD : TAsciiDiagram;
Begin
  if (Length(FRealData) = 0) or (Length(FTimes) = 0) then
    raise Exception.Create('No real and/or time data available, don''t forget ConvToReal and ConvTimes.');
  AD := TAsciiDiagram.Create(Width, Height, 5, 1, FTimes, FRealData, XDiv, YDiv);
  AD.Print;
  AD.Free;
End;

Procedure TWaveform.PrintSerialData;
Var I : Integer;
Begin
  if Length(FSerialData) = 0 then
    raise Exception.Create('No serial decode data available.');
  For I := 0 to Length(FSerialData)-1 do
    With FSerialData[I] do
      Begin
        WriteLn(Timestamp*1e6:1:3, '  ', Event);
      End;
End;

Procedure TWaveform.SaveSerialData(Filename : String);
Var I : Integer;
    T : Text;
Begin
  if Length(FSerialData) = 0 then
    raise Exception.Create('No serial decode data available.');
  Assign(T, Filename);
  Rewrite(T);
  For I := 0 to Length(FSerialData)-1 do
    With FSerialData[I] do
      Begin
        WriteLn(T,Timestamp*1e6:1:3, ',', Event);
      End;
  Close(T);
End;

Procedure TWaveform.SaveCSV(AFilename : String);
Var I : Integer;
    T : Text;
Begin
  if (Length(FRealData) = 0) or (Length(FTimes) = 0) then
    raise Exception.Create('No real and/or time data available, don''t forget ConvToReal and ConvTimes.');
  Assign(T, AFilename);
  Rewrite(T);
  if (FFormat = wfWord) and (Length(FWordData) > 0) then
    Begin
      WriteLn(T,'Index,Time[s],Word,Real[V]');
      // word data takes precedence over byte data
      For I := 0 to Length(FWordData)-1 do
        WriteLn(T, I, ',', FTimes[I], ',', FWordData[I], ',', FRealData[I]);
    End
  else if (FFormat = wfByte) and (Length(FByteData) > 0) then
    Begin
      WriteLn(T,'Index,Time[s],Byte,Real[V]');
      For I := 0 to Length(FByteData)-1 do
        WriteLn(T, I, ',', FTimes[I], ',', FByteData[I], ',', FRealData[I]);
    End
  else if (FFormat = wfAscii) and (Length(FRealData) > 0) then
    Begin
      WriteLn(T,'Index,Time[s],Real[V]');
      For I := 0 to Length(FRealData)-1 do
        WriteLn(T, I, ',', FTimes[I], ',', FRealData[I]);
    End
  else
    raise Exception.Create('Unknown waveform format or no word/byte data available.');
  Close(T);
End;

Procedure TWaveform.LoadCSV(AFilename : String);
Var T     : Text;
    St    : String;
    StArr : TDynStringArray;
    I     : Integer;
    Len   : Integer;
    Time  : Double;
Begin
  SetLength(FTimes,    0);
  SetLength(FByteData, 0);
  SetLength(FWordData, 0);
  SetLength(FRealData, 0);
  Assign(T, AFilename);
  Reset(T);
  try
    // determine file format
    ReadLn(T, St);
    if St = 'Index,Time[s],Word,Real[V]' then
      Begin
        FFormat := wfWord;
      End
    else if St = 'Index,Time[s],Byte,Real[V]' then
      Begin
        FFormat := wfByte;
      End
    else if St = 'Index,Time[s],Real[V]' then
      Begin
        FFormat := wfAscii;
      End
    else
      Begin
        raise Exception.Create('Unknown file format with first line '''+St+'''');
      End;
    I   := 0;
    Len := 0;
    While not EOF(T) do
      Begin
        ReadLn(T, St);
        StArr := SplitStr(',', St);
        if StrToInt(StArr[0]) <> I then
          raise Exception.Create('Error in line '+IntToStr(I+1+1)+': Invalid index');
        // allocate storage
        if Len < I+1 then
          Begin
            Len := Len + 1024;
            SetLength(FTimes,    Len);
            SetLength(FRealData, Len);
            if FFormat = wfByte then SetLength(FByteData, Len);
            if FFormat = wfWord then SetLength(FWordData, Len);
          End;
        // store data
        FTimes[I] := StrToFloat(StArr[1]);
        if FFormat = wfByte then
          Begin
            FByteData[I] := StrToInt  (StArr[2]);
            FRealData[I] := StrToFloat(StArr[3]);
          End
        else if FFormat = wfWord then
          Begin
            FWordData[I] := StrToInt  (StArr[2]);
            FRealData[I] := StrToFloat(StArr[3]);
          End
        else if FFormat = wfAscii then
          Begin
            FRealData[I] := StrToFloat(StArr[2]);
          End
        else
          raise Exception.Create('Unknown FFormat');
        Inc(I);
      End;
    SetLength(FTimes,    I);    // utilizing that I was incremented one last time
    SetLength(FRealData, I);
    if FFormat = wfByte then SetLength(FByteData, I);
    if FFormat = wfWord then SetLength(FWordData, I);
  finally
    Close(T);
  End;
End;

End.

