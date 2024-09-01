Unit Keithley2450;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, RemoteInstrument, KeithleyTSP;

Type
  TModel          = (mdUnknown,md2450);
  TDigits         = (dg3_5,dg4_5,dg5_5,dg6_5);
  TMeasureFunc    = (mfDCVolts,mfDCAmps,mfOhms);
  TFilterType     = (ftMovingAvg,ftRepeatAvg);
  TSenseMode      = (sm2Wire,sm4Wire);
  TSourceFunction = (sfDCVolts,sfDCAmps);
  TOffMode        = (omNormal, omZero, omHighZ, omGuard);
  TReadingUnit    = (ruDCVolts,rzDCAmps,ruOhms,ruDCWatt,ruPercent,ruLinear,ruReciprocal);

Const
  CDigits         : Array[TDigits]         of String = ('DIGITS_3_5','DIGITS_4_5','DIGITS_5_5','DIGITS_6_5');
  CMeasureFunc    : Array[TMeasureFunc]    of String = ('FUNC_DC_VOLTAGE','FUNC_DC_CURRENT','FUNC_RESISTANCE');
  CFilterType     : Array[TFilterType]     of String = ('FILTER_MOVING_AVG','FILTER_REPEAT_AVG');
  CSenseMode      : Array[TSenseMode]      of String = ('SENSE_2WIRE','SENSE_4WIRE');
  CSourceFunction : Array[TSourceFunction] of String = ('FUNC_DC_VOLTAGE','FUNC_DC_CURRENT');
  COffMode        : Array[TOffMode]        of String = ('OFFMODE_NORMAL','OFFMODE_ZERO','OFFMODE_HIGHZ','OFFMODE_GUARD');

  // bitfields of TBufferEntry.ReadingStatus
  BufRdgStatQuestionable   =   1;
  BufRdgStatOrigin         =   6;
  BufRdgStatFrontTerminal  =   8;
  BufRdgStatLimit2Low      =  16;
  BufRdgStatLimit2High     =  32;
  BufRdgStatLimit1Low      =  64;
  BufRdgStatLimit1High     = 128;
  BufRdgStatStartGroup     = 256;
  // bitfields of TBufferEntry.SourceStatus
  BufSrcStatOverVoltage    =   4;
  BufSrcStatSourceReadback =   8;
  BufSrcStatOverTemp       =  16;
  BufSrcStatSourceLimit    =  32;
  BufSrcStatSense4Wire     =  64;
  BufSrcStatOutputOn       = 128;

  CBufReadingUnit : Array[TReadingUnit]    of String = ('Volt DC','Amp DC','Ohm','Watt DC','%','mX-b','Reciprocal');
  CBufSourceUnit  : Array[TSourceFunction] of String = ('Volt DC','Amp DC');

Type

  // record for a buffer entry
  TBufferEntry = record
    Timestamp     : Double;              // UTC with fractional seconds
    Reading       : Double;
    ReadingUnit   : TReadingUnit;
    ReadingStatus : Cardinal;            // use constants BufRdgStat*
    SourceValue   : Double;
    SourceUnit    : TSourceFunction;     // use constants BufSrcStat*
    SourceStatus  : Cardinal;
  //Date          : ShortString[10];     // e.g., '03/01/2013'
  //Time          : String;              // e.g., '23:09:43'
  //Timestamp     : String;              // e.g., '03/01/2013 14:46:07.714614838'
  //Seconds       : Double;
  //FracSeconds   : Double;
  // .endindex vs. .n <-- better use .n
  End;
  // dynamic array of TBufferEntry
  TDynBufferEntryArray = Array of TBufferEntry;

  { TKeithley2450 }

  (**
   * Keithley 2450 Interactive SourceMeter SMU Instruments
   *
   * The communication with the instrument (via GPIB, ...) uses Test Script
   * Processor (TSL) commands which basically is Lua. Many predefined variables
   * and functions, in an object-oriented manner, are used to control the
   * device, e.g.
   *   smu.source.func = smu.FUNC_DC_VOLTAGE
   *   reading = smu.measure.read()
   *   print(reading)
   * All commands and variable names are case sensitive (contrary to normal
   * GPIB!).
   *
   * Since TSP is a full programming language (including if, functions, ...),
   * compared to the pure command style syntax of typical GPIB instruments
   * (e.g. the Agilent 34410A), you might want to program your own scripts
   * in TSP instead of Pascal.
   *
   * [RM-2450] p. 2.96f:
   * Using the Model 2450, you can perform the following operations:
   *  - Source voltage and measure current, voltage, resistance, or power
   *  - Source current and measure voltage, current, resistance, or power
   *  - Measure voltage, current, resistance, or power
   * When you are using a remote interface, you should set the measure function
   * first, then set the source function, because setting the measure function
   * may change the source function.
   *
   * [RM-2450] Model 2450 Interactive SourceMeter Instrument, Reference Manual,
   *      2450-901-01 Rev. D / May 2015, 2450-901-01_D_May_2015_Ref.pdf
   *)
  TKeithley2450 = class(TKeithleyTSPNodeTouch)
  protected
    FModel : TModel;
    Procedure CheckModel(IdnArr : TDynStringArray);
  protected
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Constructor Create(ATSPMaster:TKeithleyTSPNode; ANodeID:Integer);
    Destructor  Destroy; override;
    // measure functions
    Procedure SetMeasureDisplayDigits(ADigits:TDigits);
    Procedure SetMeasureFunction(AMeasureFunc:TMeasureFunc);
    Procedure SetMeasureFilterType(AFilterType:TFilterType);
    Procedure SetMeasureFilterCount(AFilterCount:Integer);
    Procedure EnableMeasureFilter(AEnable:Boolean);
    Procedure EnableMeasureAutoRange(AEnable : Boolean);
    Procedure SetMeasureAutoRangeHigh(ARange:Double);
    Procedure SetMeasureAutoRangeLow(ARange:Double);
    Procedure SetMeasureRange(ARange:Double);
    Function  GetMeasureRange:Double;
    Procedure SetNPLC(AIntegrationTime:Double);
    Function  GetNPLC:Double;
    Procedure EnableMeasureAutoZero(AEnable : Boolean);
    Procedure MeasureAutoZeroOnce;
    Procedure SetMeasureCount(ACount : Integer);
    Function  Measure(ABuffer : String) : Double;
    Function  GetBuffer(AStartIdx, AEndIdx : Integer; ABuffer : String) : TDynDoubleArray;
    Function  GetBufferAll(AStartIdx, AEndIdx : Integer; ABuffer : String) : TDynBufferEntryArray;
    Function  GetBufferLast(ABuffer : String) : TBufferEntry;
    Procedure ClearBuffer(ABuffer : String);
    //Function  Measuring : Boolean;
    // source functions
    Procedure SetSenseMode(ASenseMode:TSenseMode);
    Procedure SetSourceFunction(AFunction:TSourceFunction);
    Procedure SetSourceVoltageLimit(ALimit:Double);
    Function  GetSourceVoltageLimit:Double;
    Procedure SetSourceCurrentLimit(ALimit:Double);
    Function  GetSourceCurrentLimit:Double;
    Function  GetSourceVoltageLimitTripped : Boolean;
    Function  GetSourceCurrentLimitTripped : Boolean;
    Procedure EnableSourceAutoRange(AEnable:Boolean);
    Procedure SetSourceRange(ARange:Double);
    Function  GetSourceRange:Double;
    Procedure SetHighCapacitanceMode(AEnable:Boolean);
    Procedure EnableOutput(AEnable:Boolean);
    Procedure SetSourceOffMode(AOffMode:TOffMode);
    Procedure SetSourceLevel(ALevel : Double);
    Function  GetSouceLevel:Double;
    Procedure EnableSourceAutoDelay(AEnable:Boolean);
    Procedure SetSourceDelay(ADelay:Double);
    Function  GetSourceDelay:Double;
    property DevCom : IDeviceCommunicator read FDeviceCommunicator;
  End;


Implementation

{ TKeithley2450 }

(**
 * Connect to a Keithley 2450 series Source Meter
 *
 * Currently only the 2450 model is supported. Please add further models if you
 * have access to them.
 *)
Constructor TKeithley2450.Create(ADeviceCommunicator : IDeviceCommunicator);
Var Identity : String;
    IdnArr   : TDynStringArray;
//    I        : Integer;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  IdnArr := SplitStr(',',Identity);
//  For I := 0 to Length(IdnArr)-1 do
//    WriteLn(I,': ',IdnArr[I]);
  CheckModel(IdnArr);
  if FModel = mdUnknown then
    raise Exception.Create('Device '''+Identity+''' is not a Keithley 2450 SourceMeter SMU Instrument');
End;

Constructor TKeithley2450.Create(ATSPMaster : TKeithleyTSPNode; ANodeID : Integer);
Begin
  inherited Create(ATSPMaster, ANodeID);    // calls Create(ADeviceCommunicator : IDeviceCommunicator)
End;

Destructor TKeithley2450.Destroy;
Begin
  Inherited Destroy;
End;

(**
 * Private method to determine device model
 *)
Procedure TKeithley2450.CheckModel(IdnArr:TDynStringArray);
Begin
  FModel := mdUnknown;
  if Length(IdnArr) <> 4 then Exit;
  if UpperCase(Trim(IdnArr[0])) <> 'KEITHLEY INSTRUMENTS' then Exit;
  Case UpperCase(Trim(IdnArr[1])) of
    'MODEL 2450': FModel := md2450;
  End;
End;

(**
 * Select measurement display resolution
 *
 * [RM-2450] p. 8-117
 *)
Procedure TKeithley2450.SetMeasureDisplayDigits(ADigits : TDigits);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.displaydigits = '+FNodePrefix+'smu.'+CDigits[ADigits]);
End;

(**
 * Select measurement display function
 *
 * [RM-2450] p. 6-6, 11-3, 19-50
 *)
Procedure TKeithley2450.SetMeasureFunction(AMeasureFunc : TMeasureFunc);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.func = '+FNodePrefix+'smu.'+CMeasureFunc[AMeasureFunc]);
End;

(**
 * Set measurement filter type
 *
 * [RM-2450] p. 8-119f
 *)
Procedure TKeithley2450.SetMeasureFilterType(AFilterType : TFilterType);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.filter.type = '+CFilterType[AFilterType]);
End;

(**
 * Set number of measured readings to yield one filtered measurement
 *
 * @param AFilterCount  The number of readings required for each filtered measurement (1 to 100)
 *
 * [RM-2450] p. 8-118
 *)
Procedure TKeithley2450.SetMeasureFilterCount(AFilterCount : Integer);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.filter.count = '+IntToStr(AFilterCount));
End;

(**
 * Enable/disable filtered measurements
 *
 * [RM-2450] p. 8-118f
 *)
Procedure TKeithley2450.EnableMeasureFilter(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.filter.enable = '+FNodePrefix+'smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Enable/disable auto-range for measurement for the selected measure function
 *
 * [RM-2450] p. 8-103f
 *)
Procedure TKeithley2450.EnableMeasureAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.autorange = '+FNodePrefix+'smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Set the highest measurement range that is used when the instrument selects
 * the measurement range automatically
 *
 * The highest voltage or resistance measurement range that is used when the
 * range is set automatically:
 *  - Current:    1e-8 to 1 A
 *  - Resistance: 2 to 200e6 Ω
 *  - Voltage:    0.02 to 200 V
 *
 * [RM-2450] p. 8-104f
 *)
Procedure TKeithley2450.SetMeasureAutoRangeHigh(ARange : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.autorangehigh = '+FloatToStrF(ARange,ffExponent,8,2));
End;

(**
 * Set the lowest measurement range that is used when the instrument selects
 * the measurement range automatically
 *
 * The highest voltage or resistance measurement range that is used when the
 * range is set automatically:
 *  - Current:    1e-8 to 1 A
 *  - Resistance: 2 to 200e6 Ω
 *  - Voltage:    0.02 to 200 V
 *
 * [RM-2450] p. 8-105f
 *)
Procedure TKeithley2450.SetMeasureAutoRangeLow(ARange : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.autorangerangelow = '+FloatToStrF(ARange,ffExponent,8,2));
End;

(**
 * Set the measurement range for measurements for the selected measure function
 *
 * You can assign any real number using this command. The instrument selects the
 * closest fixed range that is large enough to measure the entered number.
 *  - Current: 1 nA to 1 A
 *  - Resistance: 20 to 200 MΩ
 *  - Voltage: 0.02 to 200 V
 *
 * [RM-2450] p. 8-136f
 *)
Procedure TKeithley2450.SetMeasureRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.range = '+FloatToStrF(ARange,ffExponent,8,2));
End;

Function TKeithley2450.GetMeasureRange : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.measure.range)'));
End;

(**
 * Set Integration aperture for measurements as multiple of power line cycles
 *
 * Range: 0.01 to 10
 *
 * [RM-2450] p. 8-124
 *)
Procedure TKeithley2450.SetNPLC(AIntegrationTime : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.nplc = '+FloatToStrF(AIntegrationTime,ffExponent,8,2));
End;

Function TKeithley2450.GetNPLC : Double;
Begin
    Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.measure.nplc)'));
End;

(**
 * Enable/disable automatic reference measurements
 *
 * [RM-2450] p. 8-106
 *)
Procedure TKeithley2450.EnableMeasureAutoZero(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.autozero = '+FNodePrefix+'smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Refresh the reference and zero measurements once.
 *
 * [RM-2450] p. 8-107
 *)
Procedure TKeithley2450.MeasureAutoZeroOnce;
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.autozero.once()');
End;

(**
 * Set number of measurements to make when a measurement is requested.
 *
 * To get better performance from the instrument, use the SimpleLoop
 * trigger-model template instead of using the count command.
 * TODO: implement
 *
 * [RM-2450] p. 8-114ff
 *)
Procedure TKeithley2450.SetMeasureCount(ACount : Integer);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.count = '+IntToStr(ACount));
End;
(**
 * Perform measurements, place them in a reading buffer, and return the last reading.
 *
 * [RM-6500] p. 14-210f
 * [RM-2450] p. 8-136
 *)
Function TKeithley2450.Measure(ABuffer : String) : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.measure.read('+ABuffer+'))'));
End;

(**
 * Retrieve measurement results from buffer
 *
 * As ABuffer use e.g. 'defbuffer1.readings', 'myTestBuf.times', ...
 *
 * TODO: use format.data = format.REAL64, format.byteorder = format.LITTLEENDIAN
 * TODO: is the datatype really always a double?
 * TODO: offer a way to retrieve multiple bufferVars
 *
 *
 * [RM-2450] p. 8-94
 *)
Function TKeithley2450.GetBuffer(AStartIdx, AEndIdx : Integer; ABuffer : String) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query(FNodePrefix+'printbuffer('+IntToStr(AStartIdx)+', '+IntToStr(AEndIdx)+', '+{FNodePrefix+'smu.'+}ABuffer+')');
  Result := SplitDouble(',',St);
End;

(**
 * Retrieve buffer entries with all information per entry
 *
 * Although the buffer has more fields (see the commented fields in
 * TBufferEntry), these do not hold additional information, just a different
 * format of existing information. Therefore these are not requested.
 *
 * If AStartIdx and AEndIdx are -1, then the last entry in the buffer is
 * retrieved. See also GetBufferLast.
 *
 *)
Function TKeithley2450.GetBufferAll(AStartIdx, AEndIdx : Integer; ABuffer : String) : TDynBufferEntryArray;
Var Q, St : String;
    A     : TDynStringArray;
    I     : Integer;
Begin
  // construct the command (query)
  Q := 'printbuffer(';
  ABuffer := FNodePrefix+ABuffer;
  if (AStartIdx = -1) and (AEndIdx = -1) then
    Q := Q + ABuffer+'.n, '+ABuffer+'.n, '
  else
    Q := Q + IntToStr(AStartIdx)+', '+IntToStr(AEndIdx)+', ';
  Q := Q + ABuffer+'.seconds, '+ABuffer+'.fractionalseconds, ';
  Q := Q + ABuffer+'.readings, '+ABuffer+'.units, '+ABuffer+'.statuses, ';
  Q := Q + ABuffer+'.sourcevalues, '+ABuffer+'.sourceunits, '+ABuffer+'.sourcestatuses)';
  // execute
  St := FDeviceCommunicator.Query(Q);
  // disseminate the reply
  A := SplitStr(',', St);
  if Length(A) mod 8 <> 0 then
    raise Exception.Create('Return string of '''+Q+''' has '+IntToStr(Length(A))+' words, which is not divisible by 8');
  SetLength(Result, Length(A) div 8);
  For I := 0 to (Length(A) div 8)-1 do
    Begin
      Result[I].Timestamp     := StrToFloat(A[I*8+0]) + StrToFloat(A[I*8+1]);
      Result[I].Reading       := StrToFloat(A[I*8+2]);
      Result[I].ReadingUnit   := TReadingUnit(Find(A[I*8+3], CBufReadingUnit));
      Result[I].ReadingStatus := StrToInt(A[I*8+4]);
      Result[I].SourceValue   := StrToFloat(A[I*8+5]);
      Result[I].SourceUnit    := TSourceFunction(Find(A[I*8+6], CBufSourceUnit));
      Result[I].SourceStatus  := StrToInt(A[I*8+7]);
    End;
End;

(**
 * Retrieve all information for the last buffer entry
 *
 *)
Function TKeithley2450.GetBufferLast(ABuffer : String) : TBufferEntry;
Begin
  Result := GetBufferAll(-1, -1, ABuffer)[0];
End;

(**
 * Clear a buffer
 *
 * [RM-2450] p. 8-17
 *)
Procedure TKeithley2450.ClearBuffer(ABuffer:String);
Begin
  FDeviceCommunicator.Send(FNodePrefix+ABuffer+'.clear()');
End;

{$IFDEF TODO_UPDATE}
(**
 * Determine if an "overlapped" measurement is currently in progress
 *
 * [RM-2450] p. 19-174
 *)
Function TKeithley2450.Measuring:Boolean;
Begin
  Result := (StrToInt(FDeviceCommunicator.Query('print(tostring(status.operation.instrument.'+FNodePrefix+'smu.condition))')) and $10 <> 0);
End;
{$ENDIF}  // TODO_UPDATE

(**
 * Set sense mode to local (2-wire) or remote (4-wire)
 *
 * Warning: This is reset to 2-wire mode if the measure function is changed.
 *
 * [RM-2450] p. 8-142
 *)
Procedure TKeithley2450.SetSenseMode(ASenseMode : TSenseMode);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.measure.sense = '+FNodePrefix+'smu.'+CSenseMode[ASenseMode]);
End;

(**
 * Set source function to voltage or current
 *
 * [RM-2450] p. 8-155
 *)
Procedure TKeithley2450.SetSourceFunction(AFunction : TSourceFunction);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.func = '+FNodePrefix+'smu.'+CSourceFunction[AFunction]);
End;

(**
 * Set maximum source voltage for operation in current source mode
 *
 * [RM] p. 8-173
 *)
Procedure TKeithley2450.SetSourceVoltageLimit(ALimit : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.vlimit.level = '+FloatToStrF(ALimit,ffExponent,8,2));
End;

Function TKeithley2450.GetSourceVoltageLimit : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.vlimit.level)'));
End;

(**
 * Set maximum source current for operation in voltage source mode
 *
 * [RM] p. 8-173
 *)
Procedure TKeithley2450.SetSourceCurrentLimit(ALimit : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.ilimit.level = '+FloatToStrF(ALimit,ffExponent,8,2));
End;

Function TKeithley2450.GetSourceCurrentLimit : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.ilimit.level)'));
End;

(**
 * Determine if the voltage limit in current mode is reached.
 *
 * If the limits were exceeded, the instrument clamps the source to keep the
 * source within the set limits.
 *
 * If you check the limit for the source that is not presently selected, nil is
 * returned.
 *
 * [RM-2450] p. 8-174
 *)
Function TKeithley2450.GetSourceVoltageLimitTripped : Boolean;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.vlimit.tripped)');
  if      St = 'smu.OFF' then Exit(False)
  else if St = 'smu.ON'  then Exit(True)
  else raise Exception.Create('Unexpected reply '''+St+'''');
End;

(**
 * Determine if the current limit in voltage mode is reached.
 *
 * [RM-2450] p. 8-174
 *)
Function TKeithley2450.GetSourceCurrentLimitTripped : Boolean;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.ilimit.tripped)');
  if      St = 'smu.OFF' then Exit(False)
  else if St = 'smu.ON'  then Exit(True)
  else raise Exception.Create('Unexpected reply '''+St+'''');
End;


(**
 * Enable/disable auto-range for source
 *
 * [RM-2450] p. 8-146f
 *)
Procedure TKeithley2450.EnableSourceAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.autorange = smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Set the source range for the selected source function.
 *
 * The fixed current source ranges are 10 nA, 100 nA, 1 µA, 10 µA, 100 µA, 1 mA, 10 mA, 100 mA, and 1 A.
 * The fixed voltage source ranges are 20 mV, 200 mV, 2 V, 20 V, and 200 V.
 *
 * To select the range, you can specify the approximate source value that you
 * will use. The instrument selects the lowest range that can accommodate that
 * level. For example, if you expect to source levels around 50 mV, send 0.05
 * (or 50e-3) to select the 200 mV range.
 *
 * [RM-2450] p. 2-114, 8-161f
 *)
Procedure TKeithley2450.SetSourceRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.range = '+FloatToStrF(ARange,ffExponent,8,2));
End;

(**
 * Get the source range for the selected source function.
 *
 * The instrument returns the positive full-scale value that the instrument is
 * presently using.
 *
 * [RM-2450] p. 2-114, 8-161f
 *)
Function TKeithley2450.GetSourceRange : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.range)'));
End;

(**
 * Enable high capacitance mode
 *
 * Note: This also changes auto-range, limit and range settings!
 *
 * [RM-2450] p. 8-156
 *)
Procedure TKeithley2450.SetHighCapacitanceMode(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.highc = '+FNodePrefix+'smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Switch on/off the source output
 *
 * See also smu.source.offmode p. 8-157
 *
 * [RM-2450] p. 8-159
 *)
Procedure TKeithley2450.EnableOutput(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.output = '+FNodePrefix+'smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Define the state of the source when the output is turned off.
 *
 * Before setting the output-off state, set the source function.
 *
 * [RM-2450] p. 8-157, p. 2-94ff
 *)
Procedure TKeithley2450.SetSourceOffMode(AOffMode : TOffMode);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.offmode = '+FNodePrefix+'smu.'+COffMode[AOffMode]);
End;

(**
 * Set source output voltage or current
 *
 * [RM-2450] p. 8-156
 *)
Procedure TKeithley2450.SetSourceLevel(ALevel : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.level = '+FloatToStrF(ALevel,ffExponent,8,2));
End;

Function TKeithley2450.GetSouceLevel : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.level)'));
End;

(**
 * Enable/disable auto-delay for source
 *
 * When autodelay is turned on, the actual delay that is set depends on the
 * range.
 * When source autodelay is on, if you set a source delay, the autodelay is
 * turned off.
 *
 * [RM-2450] p. 8-147
 *)
Procedure TKeithley2450.EnableSourceAutoDelay(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.autodelay = '+FNodePrefix+'smu.'+Select(AEnable,'ON','OFF'));
End;

(**
 * Set an additional settling time before taking a measurement
 *
 * ADelay is in Seconds.
 *
 * When either a source delay or autodelay is set, the delay is applied to the
 * first source output and then only when the magnitude of the source changes.
 *
 * [RM-2450] p. 8-154, 2-19
 *)
Procedure TKeithley2450.SetSourceDelay(ADelay : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'smu.source.delay = '+FloatToStrF(ADelay,ffExponent,8,2));
End;

(**
 * Query source delay
 *)
Function TKeithley2450.GetSourceDelay : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'smu.source.delay)'));
End;

End.

