Unit KeithleyTSP;

Interface
Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, RemoteInstrument;

Type

  TNodeInfo = record
    Node     : Integer;
    Model    : String;
    SerialNo : String;
    Version  : String;
  End;
  TNodeInfoArray = Array of TNodeInfo;

  { TKeithleyTSPNode }

  (**
   * Base class for Keithley instruments which support TSP-Link
   *
   * For the master node as well as stand-alone devices, this class behaves
   * normal. Use the constructor Create(IDeviceCommunicator).
   * For slave nodes, use the constructor Create(ATSPMaster,ANodeID). This
   * will use the master object for communication.
   *
   * All methods have to consider the variable FNodePrefix to properly work
   * in all scenarios.
   *)
  TKeithleyTSPNode = class(TRemoteInstrument)
  protected
    FNodeID     : Integer;
    FNodePrefix : String;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Constructor Create(ATSPMaster:TKeithleyTSPNode; ANodeID:Integer);
    Destructor  Destroy; override;
    { device function }
    Function  Identify : String; override;
    Function  TSPLinkInitialize    : Integer;
    Function  GetTSPLinkState      : String;
    Function  GetTSPLinkLocalNode  : Integer;
    Function  GetTSPLinkMasterNode : Integer;
    Function  GetTSPLinkNodes      : TNodeInfoArray;
    Procedure ResetAll;
    Procedure Reset;
  End;

  (**
   * Base class for Keithley instruments with a touch screen, e.g., DMM6500, 2450
   *
   *)
  TKeithleyTSPNodeTouch = class(TKeithleyTSPNode)
  public
    { device functions }
    Procedure Beep(ADuration, AFrequency : Double);
    { display functions }
    Procedure ChangeScreen(ADisplayScreen : String);
    Procedure ClearDisplay;
    Procedure SetText(ARow : Integer; AText : String);
    { event log functions }
    Procedure ClearEvents;
    Function  GetEventCount : Integer;
    Procedure GetNextEvent(Out AEventNumber : Integer; Out AMessage : String; Out ASeverity, ANodeID : Integer; Out ATime : Double);

  End;

Implementation

{ TKeithleyTSPNode }

Constructor TKeithleyTSPNode.Create(ADeviceCommunicator : IDeviceCommunicator);
Begin
  inherited Create(ADeviceCommunicator);
  FNodeID     := -1;   // without TSP or with no connected slave nodes, default to -1
  FNodePrefix := '';   // master has no prefix
End;

Constructor TKeithleyTSPNode.Create(ATSPMaster : TKeithleyTSPNode; ANodeID : Integer);
Begin
  inherited Create(ATSPMaster.FDeviceCommunicator);
  FNodeID     := ANodeID;
  FNodePrefix := 'node['+IntToStr(FNodeID)+'].';
End;

Destructor TKeithleyTSPNode.Destroy;
Begin
  inherited Destroy;
End;

Function TKeithleyTSPNode.Identify : String;
Begin
  if FNodePrefix = '' then
    Begin
      //Result := inherited Identify;
      // strange, it seems that "*IDN?" doesn't send the data, unless another string is sent, at least with USB-TMC
      FDeviceCommunicator.Send('*IDN?');
      Result := FDeviceCommunicator.Query('');
    End
  else
    Begin
      Result := FDeviceCommunicator.Query('print("KEITHLEY INSTRUMENTS,MODEL "..'+FNodePrefix+'model..","..'+FNodePrefix+'serialno..","..'+FNodePrefix+'version)');
    End;
End;

(**
 * Initialize all instruments and enclosures in the TSP-Link system
 *
 * @returns  the number of nodes found on the system, including the node on
 *           which the command is running
 *
 * [DMM6500RM] p. 14-419
 *)

Function TKeithleyTSPNode.TSPLinkInitialize : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('print(tsplink.initialize())'));
End;

Function TKeithleyTSPNode.GetTSPLinkState : String;
Begin
  Result := FDeviceCommunicator.Query('print(tsplink.state)');
End;

Function TKeithleyTSPNode.GetTSPLinkLocalNode : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('print(tsplink.node)'));
End;

Function TKeithleyTSPNode.GetTSPLinkMasterNode : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('print(tsplink.master)'));
End;

Function TKeithleyTSPNode.GetTSPLinkNodes : TNodeInfoArray;
Var St : String;
    SA : TDynStringArray;
Begin
  SetLength(Result, 0);
  FDeviceCommunicator.Send('for i = 0, 63 do if node[i] != nil then print(i .. "," .. node[i].model..","..node[i].serialno..","..node[i].version) end end print("end")');
  // I couldn't find an iterator, which skips the empty places.
  //FDeviceCommunicator.Send('for n in pairs(node) do print(n.node..","..n.model..","..n.serialno..","..n.version) end print("end")');
  repeat
    St := Trim(FDeviceCommunicator.Receive);
    //WriteLn(St);
    // Unfortunately the current implementation of IDeviceCommunicator doesn't
    // support to receive multiple lines, with an undefined end. Therefore we
    // print("end") so that in this loop we know when to stop calling Receive.
    // Otherwise a timeout exception would occur.
    if St = 'end' then break;
    SA := SplitStr(',', St);
    if Length(SA) <> 4 then
      raise Exception.Create('Invalid return string '''+St+'''');
    SetLength(Result, Length(Result)+1);
    with Result[Length(Result)-1] do
      Begin
        Node     := StrToInt(SA[0]);
        Model    := SA[1];
        SerialNo := SA[2];
        Version  := SA[3];
      End;
  Until False;
End;

Procedure TKeithleyTSPNode.ResetAll;
Begin
  FDeviceCommunicator.Send('reset()');
End;

Procedure TKeithleyTSPNode.Reset;
Begin
  FDeviceCommunicator.Send(FNodePrefix+'reset()');
End;

{ TKeithleyTSPNodeTouch }

(**
 * This function generates an audible tone.
 *
 * @param  ADuration   The amount of time to play the tone (0.001 to 100 s)
 * @param  AFrequency  The frequency of the beep (20 to 8000 Hz)
 *
 * [RM-6500] p. 14-8f
 * [RM-2450] p. 8-7
 *)
Procedure TKeithleyTSPNodeTouch.Beep(ADuration, AFrequency : Double);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'beeper.beep(' + FloatToStrF(ADuration,ffFixed,1,3) + ',' + FloatToStrF(AFrequency,ffFixed,1,3));
End;

(**
 * Change which front-panel screen is displayed.
 *
 * ADisplayScreen must be one of the documented constants like 'SCREEN_HOME',
 * 'SCREEN_USER_SWIPE', ...
 *
 * TODO: make Pascal enum and a string array
 *
 * [RM-6500] p. 14-87f
 * [RM-2450] p. 8-54
 *)
Procedure TKeithleyTSPNodeTouch.ChangeScreen(ADisplayScreen:String);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'display.changescreen(display.'+ADisplayScreen+')');
End;

(**
 * Clear the text from the front-panel USER swipe screen.
 *
 * [RM-6500] p. 14-89
 * [RM-2450] p. 8-55
 *)
Procedure TKeithleyTSPNodeTouch.ClearDisplay;
Begin
  FDeviceCommunicator.Send(FNodePrefix+'display.clear()');
End;

(**
 * Display text on the user screen
 *
 * Allowed ARow: 1, 2
 *
 * [RM-6500] p. 14-99f
 * [RM-2450] p. 8-66
 *)
Procedure TKeithleyTSPNodeTouch.SetText(ARow:Integer;AText : String);
Begin
  FDeviceCommunicator.Send(FNodePrefix+'display.settext(display.TEXT'+IntToStr(ARow)+', "'+AText+'")');
End;

(**
 * Clear the event log
 *
 * [RM-6500] p. 14-248
 * [RM-2450] p. 8-68
 *)
Procedure TKeithleyTSPNodeTouch.ClearEvents;
Begin
  FDeviceCommunicator.Send(FNodePrefix+'eventlog.clear()');
End;

(**
 * Get number of unread events in the event log
 *
 * [RM-6500] p. 14-249
 * [RM-2450] p. 8-68f
 *)
Function TKeithleyTSPNodeTouch.GetEventCount : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('print('+FNodePrefix+'eventlog.getcount())'));
End;

(**
 * Get oldest unread event message from the event log
 *
 * [RM-6500] p. 14-250f
 * [RM-2450] p. 8-69f
 *)
Procedure TKeithleyTSPNodeTouch.GetNextEvent(Out AEventNumber : Integer; Out AMessage : String; Out ASeverity, ANodeID : Integer; Out ATime : Double);
Var TimeSeconds, TimeNanoSec : Integer;
Begin
  FDeviceCommunicator.Send('eventNumber, message, severity, nodeID, timeSeconds, timeNanoSeconds = eventlog.next()');
  AEventNumber := StrToInt(FDeviceCommunicator.Query('print(eventNumber)'));
  AMessage     :=          FDeviceCommunicator.Query('print(message)');
  ASeverity    := StrToInt(FDeviceCommunicator.Query('print(severity)'));
  ANodeID      := StrToInt(FDeviceCommunicator.Query('print(nodeID)'));
  TimeSeconds  := StrToInt(FDeviceCommunicator.Query('print(timeSeconds)'));
  TimeNanoSec  := StrToInt(FDeviceCommunicator.Query('print(timeNanoSeconds)'));
  ATime := 1.0*TimeSeconds + 1E-9*TimeNanoSec;
End;

End.

