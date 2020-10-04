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
  TWaveformSource     = (wsCH1,wsCH2,wsCH3,wsCH4,
                         wsPOD1,wsPOD2,wsBus1,wsBus2,
                         wsFunction,wsMath,wsWMemory1,wsWMemory2,wsSBus1,SBus2);
  TWaveformSubSource  = (wsSub0,wsSub1);
  TWaveformFormat     = (wfByte, wfWord, wfAscii);
  TWaveformType       = (wtNormal, wtPeak, wtAverage, wtHighRes);
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
  CWaveformSource     : Array[TWaveformSource]     of String = ('CHAN1','CHAN2','CHAN3','CHAN4',
                                                                'POD1','POD2','BUS1','BUS2',
                                                                'FUNC','MATH','WMEM1','WMEM2','SBUS1','SBUS2');
  CWaveformSubSource  : Array[TWaveformSubSource]  of String = ('SUB0','SUB1');
  CWaveformFormat     : Array[TWaveformFormat]     of String = ('BYTE', 'WORD', 'ASC');
  CWaveformType       : Array[TWaveformType]       of String = ('NORM', 'PEAK', 'AVER','HRES');
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
    FType       : TWaveformType;
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
    Procedure PrintPreamble;
    Procedure ConvToReal;
    Function  GetTime(Index:Integer) : Double;
    Procedure PrintAsciiArt(Width, Height : Integer; XDiv, YDiv : Double);
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
    Function  QueryBinary(Cmd : String) : TDynByteArray;
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
    // Status
    Function  GetOperationStatusCondition : Word;
    // Cursor
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
 * Query command which expects IEEE-488.2 binary block data
 *
 *)
Function TAgilentMSOX3000A.QueryBinary(Cmd : String) : TDynByteArray;
Var St : String;
    L  : Integer;

  // function to be called by WaitTimeout multiple times until enough data was recieved or a timeout has happened
  Function ReceiveData(Data:Pointer) : Boolean;
  Begin
    St := St + FDeviceCommunicator.Receive;
    if (L = 0) and (Length(St) >= 10) then
      Begin
        // received enough to have the full length information
        if (St[1] <> '#') or (St[2] <> '8') then
          raise Exception.Create('Received invalid data format');
        L := StrToInt(Copy(St,3,8));
      End;
    Result := (Length(St) >= 10+L+1);
  End;

Begin
  // not using FDeviceCommunicator.Query because firstly it is limited by the
  // USBTMC (and perhaps others) transfer size, and secondly because it trims
  // the received string.
  FDeviceCommunicator.Send(Cmd);
  (* The return format is '#8<byte_length><binary_block>'+LF
   * where <byte_length> are 8 digits (with leading zeros) specifying the number
   * of bytes in <binary_block>.
   *)
  St := '';
  L  := 0;
  WaitTimeout(0, 1, (FDeviceCommunicator.GetTimeout div 1000)+30000, @ReceiveData, Nil);
  // check
  //WriteLn('Received '+IntToStr(Length(St))+' bytes, expected '+IntToStr(10+L+1)+' ('+IntToStr(L)+' for image data)');
  if Length(St) <> 10+L+1 then
    raise Exception.Create('Received '+IntToStr(Length(St))+' bytes but expected '+IntToStr(10+L+1)+' ('+IntToStr(L)+' for data)');
  if St[Length(St)] <> #10 then
    raise Exception.Create('Received invalid data format');
  //Dump(St[1], Length(St));
  // just use the data starting at the 11th byte
  SetLength(Result,L);
  Move(St[11],Result[0],L);
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
Var St : String;
    L  : LongInt;
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
 * [PG] p. 969
 *)
Procedure TAgilentMSOX3000A.SetWaveformFormat(AFormat : TWaveformFormat);
Begin
  FDeviceCommunicator.Send(':WAVEFORM:FORMAT '+CWaveformFormat[AFormat])
End;

(**
 * Query swaveform transmission format
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
 * Set number of points to be retrieved for a waveform
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
    '+0' : Result.FType := wtNormal;
    '+1' : Result.FType := wtPeak;
    '+2' : Result.FType := wtAverage;
    '+3' : Result.FType := wtHighRes;
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
Begin
  AWaveform.FByteData := QueryBinary(':WAVEFORM:DATA?');
  if AWaveform.FFormat = wfWord then
    Begin
      // copy to FWordData
      SetLength(AWaveform.FWordData, Length(AWaveform.FByteData) shr 1);
      Move(AWaveform.FByteData[0], AWaveform.FWordData[0], Length(AWaveform.FByteData));
      SetLength(AWaveform.FByteData, 0);
    End
  else if AWaveform.FFormat = wfAscii then
    Begin
      // copy to String
      SetLength(St, Length(AWaveform.FByteData));
      Move(AWaveform.FByteData[0], St[1], Length(AWaveform.FByteData));
      SetLength(AWaveform.FByteData, 0);
      // split
      AWaveform.FRealData := SplitDouble(',', St);
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

Procedure TWaveform.PrintPreamble;
Begin
  WriteLn('Source    : ', CWaveformSource    [FSource    ]);               // TWaveformSource;
  WriteLn('SubSource : ', CWaveformSubSource [FSubSource ]);               // TWaveformSubSource;
  WriteLn('Format    : ', CWaveformFormat    [FFormat    ]);               // TWaveformFormat;
  WriteLn('Type      : ', CWaveformType      [FType      ]);               // TWaveformType;
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
  if FType <> wtPeak then
    Result := ((Index - FXReference) * FXIncrement) + FXOrigin
  else
    Result := ((Index - FXReference) * 2 * FXIncrement) + FXOrigin;
End;

Procedure TWaveform.PrintAsciiArt(Width, Height : Integer; XDiv,YDiv:Double);
Var Min,Max : Double;
    AD : TAsciiDiagram;
    I  : Integer;
Begin
  if Length(FRealData) = 0 then Exit;
  Min := MinValue(FRealData);
  Max := MaxValue(FRealData);
  Min := Min - (Max-Min)*0.05;
  Max := Max + (Max-Min)*0.05;
  AD := TAsciiDiagram.Create(Width, Height, 5, 1, GetTime(0), GetTime(Length(FRealData)-1)*1.02, Min, Max);
  AD.DrawAxes;
  AD.DrawXTics(XDiv);
  AD.DrawXTics(YDiv);
  For I := 0 to Length(FRealData)-1 do
    AD.Put(GetTime(I), FRealData[I], '*');
  AD.Print;
  AD.Free;
End;

End.

