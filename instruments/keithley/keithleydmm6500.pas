Unit KeithleyDMM6500;

Interface
Uses
  Classes, SysUtils, StrUtils,
  PasGpibUtils, DevCom, RemoteInstrument, Instrument,
  KeithleyTSP;

Type
  TInputTerminalsSetting = (itFront,itRear);
  TDigits         = (dg3_5,dg4_5,dg5_5,dg6_5);

Const
  CInputTerminalsSettingNice : Array[TInputTerminalsSetting] of String = ('Front','Rear');
  CDigits         : Array[TDigits]         of String = ('DIGITS_3_5','DIGITS_4_5','DIGITS_5_5','DIGITS_6_5');

Type
  (**
   * Keithley DMM6500 6 1/2 Digit Multimeter
   *
   * The communication with the instrument (via GPIB, USB, LAN) uses Test Script
   * Processor commands, which are Lua. Many predefined variables and functions,
   * functions, in an object-oriented manner, are used to control the device,
   * e.g.
   *   dmm.measure.func = dmm.FUNC_DC_VOLTAGE
   *   reading = dmm.measure.read()
   *   print(reading)
   * All commands and variable names are case sensitive (contrary to normal
   * GPIB SCPI!).
   *
   * Since TSP commands use a full programming language (including if,
   * functions, ...), compared to the pure command style syntax of typical GPIB
   * SCPI instruments (e.g. the Agilent 34410A), you might want to program your
   * own scripts instead of Pascal.
   *
   * [RM-6500] Model DMM6500 6½-Digit Multimeter with Scanning Reference Manual,
   *           DMM6500-901-01 Rev. B / September 2019
   *)

  { TKeithleyDMM6500 }

  TKeithleyDMM6500 = class(TKeithleyTSPNodeTouch)
  private
    FIdentity : String;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Constructor Create(ATSPMaster:TKeithleyTSPNode; ANodeID:Integer);
    Destructor  Destroy; override;
  protected
    Procedure CheckModel;
  public
    { device function }
    { buffer functions }
    Procedure ClearBuffer(ABuffer:String='');
    Function  GetNumReadings(ABuffer:String='') : Integer;
    Function  PrintBuffer(ABuffer:String='';AEndIndex:Integer=0;AStartIndex:Integer=1) : TDynDoubleArray;
    { measure functions }
    Procedure SetAperture(AAperture:Double);
    Function  GetAperture : Double;
    Procedure EnableAutoRange(AEnable:Boolean);
    Procedure EnableAutoZero(AEnable:Boolean);
    Procedure AutoZero;
    Procedure SetMeasureCount(ACount:Integer);
    Procedure SetDisplayDigits(ADigits : TDigits);
    Procedure EnableFilter(AEnable : Boolean);
    Procedure SetMeasureFunction(AFunction:String{TMeasureFunc});
    Procedure SetNPLC(ANPLC:Double);
    Function  GetNPLC : Double;
    Procedure SetRange(ARange:Double);
    Function  GetRange : Double;
    Function  Measure(ABuffer:String = '') : Double;
    Function  GetTerminals : TInputTerminalsSetting;
    class Function GetRanges(AInstrument:String) : TRangesQuantity; override;
  End;

Implementation

{ TKeithleyDMM6500 }

Constructor TKeithleyDMM6500.Create(ADeviceCommunicator : IDeviceCommunicator);
Begin
  inherited Create(ADeviceCommunicator);
  CheckModel;
End;

Constructor TKeithleyDMM6500.Create(ATSPMaster : TKeithleyTSPNode; ANodeID : Integer);
Begin
  inherited Create(ATSPMaster, ANodeID);
End;

Destructor TKeithleyDMM6500.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TKeithleyDMM6500.CheckModel;
Var IdnArr : TDynStringArray;
    I      : Integer;
Begin
  { check device }
  FIdentity := Identify;
//  WriteLn('Identify: ''',FIdentity,'''');
  IdnArr := SplitStr(',',FIdentity);
//  For I := 0 to Length(IdnArr)-1 do
//    WriteLn(I,': ',IdnArr[I]);
  if (Length(IdnArr) = 4) and
     ((IdnArr[0] = 'KEITHLEY INSTRUMENTS') and (IdnArr[1] = 'MODEL DMM6500')) then
    Exit;
  raise Exception.Create('Device '''+FIdentity+''' is not a supported device');
End;

(**
 * Clear all readings and statistics from the specified buffer
 *
 * [RM-6500] p. 14-35f
 *)
Procedure TKeithleyDMM6500.ClearBuffer(ABuffer : String);
Begin
  if ABuffer = '' then ABuffer := 'defbuffer1';
  ABuffer := FNodePrefix+ABuffer;
  FDeviceCommunicator.Send(ABuffer+'.clear()');
End;

(**
 * Get the number of readings in the specified reading buffer.
 *
 * [RM-6500] p. 14-46f
 *)
Function TKeithleyDMM6500.GetNumReadings(ABuffer : String) : Integer;
Begin
  if ABuffer = '' then ABuffer := 'defbuffer1';
  ABuffer := FNodePrefix+ABuffer;
  Result := StrToInt(FDeviceCommunicator.Query('print('+ABuffer+'.n)'));
End;

(**
 * Get the readings in the specified reading buffer.
 *
 * [RM-6500] p. 14-47
 *)
Function TKeithleyDMM6500.PrintBuffer(ABuffer : String; AEndIndex : Integer; AStartIndex : Integer) : TDynDoubleArray;
Var EndIndex : String;
    St       : String;
Begin
  if ABuffer = '' then ABuffer := 'defbuffer1';
  ABuffer := FNodePrefix+ABuffer;
  if AEndIndex < 1 then
    EndIndex := ABuffer+'.n'
  else
    EndIndex := IntToStr(AEndIndex);
  FDeviceCommunicator.Send('printbuffer('+IntToStr(AStartIndex)+', '+EndIndex+', '+ABuffer+'.readings)');
  St := FDeviceCommunicator.Query('');
  // strange, it seems that "printbuffer()" doesn't send the data, unless another string is sent, at least with USB-TMC
  Result := SplitDouble(',',St);
End;

(**
 * Set aperture (integration time) for the selected measurement function.
 *
 * [RM-6500] p. 14-154ff
 *)
Procedure TKeithleyDMM6500.SetAperture(AAperture : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.aperture = '+FloatToStr(AAperture));
End;

(**
 * Get aperture (integration time) for the selected measurement function.
 *
 * [RM-6500] p. 14-154ff
 *)
Function TKeithleyDMM6500.GetAperture : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'dmm.measure.aperture)'));
End;

(**
 * Enable/disable auto range
 *
 * [RM-6500] p. 14-157f
 *)
Procedure TKeithleyDMM6500.EnableAutoRange(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.autorange = '+FNodePrefix+'dmm.'+IfThen(AEnable,'ON','OFF'));
End;

(**
 * Enable/disable automatic updates to the internal reference measurements (autozero).
 *
 * [RM-6500] p. 14-159f
 *)
Procedure TKeithleyDMM6500.EnableAutoZero(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.autozero.enable = '+FNodePrefix+'dmm.'+IfThen(AEnable,'ON','OFF'));
End;

(**
 * Refresh the reference and zero measurements once.
 *
 * [RM-6500] p. 14-160
 *)
Procedure TKeithleyDMM6500.AutoZero;
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.autozero.once()');
End;

(**
 * Set number of measurements to make when a measurement is requested.
 *
 * To get better performance from the instrument, use the SimpleLoop
 * trigger-model template instead of using the count command.
 * TODO: implement
 *
 * [RM-6500] p. 14-169f
 *)
Procedure TKeithleyDMM6500.SetMeasureCount(ACount : Integer);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.count = '+IntToStr(ACount));
End;

(**
 * This attribute determines the number of digits that are displayed for
 * measurements on the front panel.
 *
 * This command affects how the reading for a measurement is displayed on the
 * front panel of the instrument. It does not affect the number of digits
 * returned in a remote command reading. It also does not affect the accuracy
 * or speed of measurements.
 *
 * [RM-6500] p. 14-174f
 *)
Procedure TKeithleyDMM6500.SetDisplayDigits(ADigits : TDigits);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.displaydigits = '+FNodePrefix+'dmm.'+CDigits[ADigits]);
End;

(**
 * Enable or disable the averaging filter for measurements of the selected function
 *
 * [RM-6500] p. 14-176f
 *)
Procedure TKeithleyDMM6500.EnableFilter(AEnable : Boolean);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.filter.enable = '+FNodePrefix+'dmm.'+IfThen(AEnable,'ON','OFF'));
End;

(**
 * Set the active measure function.
 *
 * TODO: make Pascal enum and a string array
 *
 * [RM-6500] p. 14-181ff
 *)
Procedure TKeithleyDMM6500.SetMeasureFunction(AFunction : String);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.func = '+FNodePrefix+'dmm.'+AFunction);
End;

(**
 * Set integration time in multiples of power line cycles.
 *
 * [RM-6500] p. 14-204f
 *)
Procedure TKeithleyDMM6500.SetNPLC(ANPLC : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.nplc = '+FloatToStr(ANPLC));
End;

(**
 * Query the integration time in multiples of power line cycles.
 *
 * [RM-6500] p. 14-204f
 *)
Function TKeithleyDMM6500.GetNPLC : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'dmm.measure.nplc)'));
End;

(**
 * Set the positive full-scale measure range.
 *
 * [RM-6500] p. 14-297ff
 *)
Procedure TKeithleyDMM6500.SetRange(ARange : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'dmm.measure.range = '+FloatToStr(ARange));
End;

(**
 * Query the positive full-scale measure range.
 *
 * [RM-6500] p. 14-297ff
 *)
Function TKeithleyDMM6500.GetRange : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'dmm.measure.range)'));
End;

(**
 * Perform measurements, place them in a reading buffer, and return the last reading.
 *
 * [RM-6500] p. 14-210f
 *)
Function TKeithleyDMM6500.Measure(ABuffer : String) : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('print('+FNodePrefix+'dmm.measure.read('+ABuffer+'))'));
End;

(**
 * Get which set of input and output terminals the instrument is using.
 *
 * [RM-6500] p. 14-248
 *)
Function TKeithleyDMM6500.GetTerminals : TInputTerminalsSetting;
Var Terminals : String;
Begin
  Terminals := FDeviceCommunicator.Query('print('+FNodePrefix+'dmm.terminals)');
  if      Terminals = 'dmm.TERMINALS_FRONT' then Exit(itFront)
  else if Terminals = 'dmm.TERMINALS_REAR'  then Exit(itRear)
  else
    raise Exception.Create('Unknown terminal location '''+Terminals+''' returned from device.');
End;


Class Function TKeithleyDMM6500.GetRanges(AInstrument : String) : TRangesQuantity;
Begin
  Case AInstrument of
    'KeithleyDMM6500' : Begin
      SetLength(Result[qtDCV], 5);          // 1 year within calibration
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(   0.1, true, 100E-9); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.0030*0.01, 0.0035*0.01*   0.1));
      Result[qtDCV][1] := TMeasureRangeAccuracy.Create(   1.0, true,   1E-6); Result[qtDCV][1].AddAccuracy(TAccuracyGainOffset.Create(0.0025*0.01, 0.0006*0.01*   1.0));
      Result[qtDCV][2] := TMeasureRangeAccuracy.Create(  10.0, true,  10E-6); Result[qtDCV][2].AddAccuracy(TAccuracyGainOffset.Create(0.0025*0.01, 0.0005*0.01*  10.0));
      Result[qtDCV][3] := TMeasureRangeAccuracy.Create( 100.0, true, 100E-6); Result[qtDCV][3].AddAccuracy(TAccuracyGainOffset.Create(0.0040*0.01, 0.0006*0.01* 100.0));
      Result[qtDCV][4] := TMeasureRangeAccuracy.Create(1000.0, true,   1E-3); Result[qtDCV][4].AddAccuracy(TAccuracyGainOffset.Create(0.0040*0.01, 0.0006*0.01*1000.0));
      WriteLn('Warning: TODO: Implement for other accuracy cases and for other quantities');
    End;
  Else
    raise Exception.Create('TODO: Implement instrument '''+AInstrument+'''');
  End;
End;

End.

