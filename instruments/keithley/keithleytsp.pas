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
      Result := FDeviceCommunicator.Query('print("KEITHLEY INSTRUMENTS,MODEL "..localnode.model..","..localnode.serialno..","..localnode.version)');
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

End.

