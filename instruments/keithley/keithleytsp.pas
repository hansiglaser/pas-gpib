Unit KeithleyTSP;

Interface
Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, RemoteInstrument;

Type

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
    Function Identify : String; override;
    Procedure TSPLinkInitialize;
    Function  GetTSPLinkState : String;
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

Procedure TKeithleyTSPNode.TSPLinkInitialize;
Begin
  FDeviceCommunicator.Send('tsplink.initialize()');
End;

Function TKeithleyTSPNode.GetTSPLinkState : String;
Begin
  Result := FDeviceCommunicator.Query('print(tsplink.state)');
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

