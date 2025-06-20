(**
 * Driver for Keysight E36300 and E36200 Series Programmable DC Power Supplies
 *
 * This driver supports the devices
 *   E36311A  80 W Economy Model
 *   E36312A  80 W Full-Featured Model
 *   E36313A 160 W High Current Model
 *   E36231A 200W Autoranging power supply 30V, 20A
 *   E36232A 200W Autoranging power supply 60V, 10A
 *   E36233A 400W Dual Output Autoranging power supply 30V, 20A
 *   E36234A 400W Dual Output Autoranging power supply 60V, 10A
 *
 * It was originally implemented for the E36300 series and later updated for
 * the newer E36200 Series.
 *
 * The driver does not do any sanitizing of the parameters, e.g., allowed
 * channels, allowed voltage/current ranges, ...
 *
 * The SCPI commands for the E36200 Series are near identical to the E36300
 * Series, with only a few small changes:
 *  - Channel Names: P6V, P25V, N25V = CH1, CH2, CH3 → CH1 and CH2
 *  - a few more errors -200..-215, -250..-258, -270..-363
 *  - new commands CURRent:STEP, VOLTage:STEP
 *  - removed DIAGnostic Subsystem (enables or disables the current low range)
 *  - slight differences for APPLy Subsystem
 *  - new value "UP" and "DOWN" for CURRent, new command CURRent:STEP
 *  - new value "RELay" for DIGital:PIN<1-3>:FUNCtion
 *  - "DISPlay:VIEW" changed from "METER1 or METER3" to "METER1 or METER2"
 *  - "INITiate:DLOG" has different requirements on the filename
 *  - removed "INSTrument:COUPle[:TRIGger]", which is only supported in E3631A persona mode.
 *  - "INSTrument:NSELect 1 | 2 | 3" → "1 | 2"
 *  - LIST:COUNt maximum increased from 256 to 9999
 *  - new command OUTPut:PMODe: on or output off transitions, optimized for either constant voltage or constant current operation, overshoots are minimized
 *  - new command OUTPut:RELay (for an external relais)
 *  - for "SENSe:DLOG:TIME", the maximum reduced from 30000 hours and 7MB to 21,845 hours and 20 minutes and 5MB
 *  - new commads "STATus:OPERation:CONDition?", "STATus:OPERation[:EVENt]?", and "STATus:OPERation:ENABle"
 *  - new command "STATus:QUEStionable:CONDition?"
 *  - removed command "SYSTem:COMMunicate:TCPip:CONTrol?"
 *  - changed settings for "SYSTem:PERSona:MODel"
 *  - new value "UP" and "DOWN" for VOLTage, new command VOLTage:STEP
 *  - new commands "VOLTage:PROTection:STATe"
 *  - different values for "VOLTage:RANGe", but "only applicable during persona mode."
 *  - new commands "VOLTage:SLEW:FALLing", "VOLTage:SLEW:FALLing:MAXimum",
 *)
Unit KeysightE3631xA;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, Instrument, RemoteInstrument;

Type
  TChannel    = 1..3;
  TChannelSet = set of TChannel;
  TPairMode   = (pmOff,pmParallel,pmSeries);
  TSenseMode  = (smLocal,smRemote);

Const
  CPairMode       : Array[TPairMode]  of String = ('OFF','PARALLEL','SERIES');
  CPairModeShort  : Array[TPairMode]  of String = ('OFF','PAR','SER');
  CSenseMode      : Array[TSenseMode] of String = ('INTERNAL','EXTERNAL');
  CSenseModeShort : Array[TSenseMode] of String = ('INT','EXT');

Type

  { TKeysightE3631xA }

  TKeysightE3631xA = class(TRemoteInstrument)
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
    Function  MeasureVoltage(Ch : TChannel) : Double;
    Function  MeasureVoltage(Ch : TChannelSet) : TDynDoubleArray;
    Function  MeasureCurrent(Ch : TChannel) : Double;
    Function  MeasureCurrent(Ch : TChannelSet) : TDynDoubleArray;
    Procedure EnableOutput(Ch : TChannelSet; State : Boolean);
    Function  GetPairMode : TPairMode;
    Procedure SetPairMode(PairMode : TPairMode);
    Procedure SetTrackMode(TrackMode : Boolean);
    Function  GetVoltage(Ch : TChannel) : Double;
    Procedure SetVoltage(Ch : TChannelSet; Voltage : Double);
    Function  GetCurrentLimit(Ch : TChannel) : Double;
    Procedure SetCurrentLimit(Ch : TChannelSet; Current : Double);
    Function  GetSenseMode(Ch : TChannel) : TSenseMode;
    Procedure SetSenseMode(Ch : TChannelSet; SenseMode : TSenseMode);
    class Function GetRanges(AInstrument:String) : TRangesQuantity; override;
  Const
    AccIdxV = 0;
    AccIdxI = 1;
    AccIdxIlow = 2;
  End;

Implementation

{ TKeysightE3631xA }

Constructor TKeysightE3631xA.Create(ADeviceCommunicator : IDeviceCommunicator);
Var Identity : String;
    IdnArr   : TDynStringArray;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
//  WriteLn(Identity);   // e.g. 'Keysight Technologies,34461A,MY12345678,A.02.17-02.40-02.17-00.52-04-02'
  IdnArr := SplitStr(',',Identity);
//  For I := 0 to Length(IdnArr)-1 do
//    WriteLn(I,': ',IdnArr[I]);
  if not IsSupportedDevice(IdnArr) then
    raise Exception.Create('Device '''+Identity+''' is not a supported device');
End;

Destructor TKeysightE3631xA.Destroy;
Begin
  Inherited Destroy;
End;

Function TKeysightE3631xA.IsSupportedDevice(IdnArr : TDynStringArray) : Boolean;
Begin
  Result := (Length(IdnArr) = 4) and
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36311A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36312A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36313A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36231A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36232A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36233A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36234A'));
End;

Function  TKeysightE3631xA.GetChannelList(Ch:TChannel) : String;
Begin
  Result := '(@' + IntToStr(Ch) + ')';
End;

Function  TKeysightE3631xA.GetChannelList(Ch:TChannelSet) : String;
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
 * see [E36300Prg] p. 25ff
 *)
Procedure TKeysightE3631xA.Reset;
Begin
  FDeviceCommunicator.Send('*RST');
End;

(**
 * Enable/disable the beeper
 *)
Procedure TKeysightE3631xA.SetBeeper(Enable : Boolean);
Begin
  FDeviceCommunicator.Send('SYSTEM:BEEPER:STATE ' + Select(Enable,'ON','OFF'));
End;

Function TKeysightE3631xA.GetNextError : String;
Begin
  Result := FDeviceCommunicator.Query('SYSTEM:ERROR?');
End;

Procedure TKeysightE3631xA.GetNextError(Out Code : Integer; Out Msg : String);
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
 * Measure the voltage of one channel
 *
 * [E36300Prg] p. 69
 *)
Function TKeysightE3631xA.MeasureVoltage(Ch : TChannel) : Double;
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
 * [E36300Prg] p. 69
 *)
Function TKeysightE3631xA.MeasureVoltage(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:VOLTAGE? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Measure the current of one channel
 *
 * [E36300Prg] p. 69
 *)
Function TKeysightE3631xA.MeasureCurrent(Ch : TChannel) : Double;
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
 * [E36300Prg] p. 69
 *)
Function TKeysightE3631xA.MeasureCurrent(Ch : TChannelSet) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('MEASURE:CURRENT? '+GetChannelList(Ch));
  Result := SplitDouble(',',St);
End;

(**
 * Switch outputs on or off
 *
 * State: true: on, false: off
 *
 * [E36300Prg] p. 71
 *)
Procedure TKeysightE3631xA.EnableOutput(Ch : TChannelSet;State:Boolean);
Begin
  FDeviceCommunicator.Send('OUTPUT:STATE '+Select(State,'ON','OFF')+', '+GetChannelList(Ch));
End;

(**
 * Get output pairing mode
 *)
Function TKeysightE3631xA.GetPairMode : TPairMode;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('OUTPUT:PAIR?');
  For Result in TPairMode do
    if St = CPairModeShort[Result] then
      Exit;
  raise Exception.Create('Invalid return string '''+St+'''');
End;

(**
 * Set output pairing mode
 *
 * [E36300Prg] p. 73
 *)
Procedure TKeysightE3631xA.SetPairMode(PairMode : TPairMode);
Begin
  FDeviceCommunicator.Send('OUTPUT:PAIR '+CPairMode[PairMode]);
End;

(**
 * Enable/disable output tracking, i.e., Ch3 = Ch2
 *
 * [E36300Prg] p. 74
 *)
Procedure TKeysightE3631xA.SetTrackMode(TrackMode : Boolean);
Begin
  FDeviceCommunicator.Send('OUTPUT:TRACK:STATE '+Select(TrackMode,'ON','OFF'));
End;

(**
 * Get setting of output voltage
 *)
Function TKeysightE3631xA.GetVoltage(Ch : TChannel) : Double;
Var St : String;
    J  : Integer;
Begin
  St := FDeviceCommunicator.Query('SOURCE:VOLTAGE? '+GetChannelList(Ch));
  Val(St,Result,J);
  if J <> 0 then
    raise Exception.CreateFmt('Invalid floating point number ''%s'' at position %d',[St,J]);
End;

(**
 * Set output voltage
 *
 * [E36300Prg] p. 89
 *)
Procedure TKeysightE3631xA.SetVoltage(Ch : TChannelSet; Voltage : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:VOLTAGE '+FloatToStrF(Voltage,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Get current limit
 *)
Function TKeysightE3631xA.GetCurrentLimit(Ch : TChannel) : Double;
Var St : String;
    J  : Integer;
Begin
  St := FDeviceCommunicator.Query('SOURCE:CURRENT? '+GetChannelList(Ch));
  Val(St,Result,J);
  if J <> 0 then
    raise Exception.CreateFmt('Invalid floating point number ''%s'' at position %d',[St,J]);
End;

(**
 * Set the current limit
 *
 * [E36300Prg] p. 44
 *)
Procedure TKeysightE3631xA.SetCurrentLimit(Ch : TChannelSet; Current : Double);
Begin
  FDeviceCommunicator.Send('SOURCE:CURRENT '+FloatToStrF(Current,ffFixed,1,5)+', '+GetChannelList(Ch));
End;

(**
 * Get current setting of local or remote sensing
 *)
Function TKeysightE3631xA.GetSenseMode(Ch : TChannel) : TSenseMode;
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
 * [E36300Prg] p. 01
 *)
Procedure TKeysightE3631xA.SetSenseMode(Ch : TChannelSet; SenseMode : TSenseMode);
Begin
  FDeviceCommunicator.Send('SOURCE:VOLTAGE:SENSE '+CSenseMode[SenseMode]+', '+GetChannelList(Ch));
End;

Class Function TKeysightE3631xA.GetRanges(AInstrument : String) : TRangesQuantity;
Begin
  SetLength(Result[qtDCV], 1);
  Case AInstrument of
    'KeysightE36313A:S:Ch1' : Begin       // source programming Ch1
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(   6.0, false, 0.36E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.03*0.01, 3E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.05*0.01, 4E-3));   // AccIdxI
    End;
    'KeysightE36313A:S:Ch2',
    'KeysightE36313A:S:Ch3' : Begin       // source programming Ch2, Ch3
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  25.0, false, 1.50E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.03*0.01, 5E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 3E-3));   // AccIdxI
    End;
    'KeysightE36313A:S:Ch2||3' : Begin    // source programming Ch2 || Ch3 (parallel)
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  25.0, false, 1.50E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.03*0.01, 5E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 3E-3));   // AccIdxI
    End;
    'KeysightE36313A:S:Ch2+3' : Begin    // source programming Ch2 + Ch3 (series)
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  50.0, false, 3.00E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.03*0.01,10E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 6E-3));   // AccIdxI
    End;
    'KeysightE36313A:M:Ch1' : Begin
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(   6.0, false, 0.24E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 3E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.05*0.01, 5E-3));   // AccIdxI
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.25*0.01,80E-6));   // AccIdxIlow < 20mA
    End;
    'KeysightE36313A:M:Ch2',
    'KeysightE36313A:M:Ch3' : Begin
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  25.0, false, 1.00E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 5E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 3E-3));   // AccIdxI
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.25*0.01,80E-6));   // AccIdxIlow < 10mA
    End;
    'KeysightE36313A:M:Ch2||3' : Begin
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  25.0, false, 1.00E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 5E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 3E-3));   // AccIdxI
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.25*0.01,80E-6));   // AccIdxIlow < 10mA
    End;
    'KeysightE36313A:M:Ch2+3' : Begin
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  50.0, false, 2.00E-3); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01,10E-3));   // AccIdxV
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.04*0.01, 6E-3));   // AccIdxI
                                                                                Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.25*0.01,80E-6));   // AccIdxIlow < 10mA
    End;
  else
    raise Exception.Create('TODO: Implement instrument '''+AInstrument+'''');
  End;
End;

End.

