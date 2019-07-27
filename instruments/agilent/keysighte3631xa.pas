Unit KeysightE3631xA;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, RemoteInstrument;

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
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = 'E36313A'));
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

End.

