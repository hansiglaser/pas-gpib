Unit DevComTCPLeCroy;

{$mode objfpc}{$H+}

// use interfaces without IUnknown as ancestor, so they don' have reference
// counting, the classes don't need to be derived from TInterfacedObject, and
// there are no strange problems when destroying objects using our classes
{$interfaces corba}

Interface

Uses
  Classes, SysUtils,
  DevComTCP;

Type
  { TTCPLeCroyCommunicator }

  (**
   * Communicate to a remote instrument via TCP/IP using a special header for
   * certian LeCroy devices. This protocol is called LeCroy-VICP. Google found
   * the following links:
   *  - http://optics.eee.nottingham.ac.uk/lecroy_tcp/
   *  - http://sourceforge.net/projects/lecroyvicp/
   *  - http://sourceforge.net/tracker/?func=detail&atid=727721&aid=1681732&group_id=133616
   *)
  TTCPLeCroyCommunicator = class(TTCPCommunicator)
  private
    FIndex : Integer;
  public
    (**
     * Instanciate and open TCP/IP connection
     *
     * @param AHost    host name or IP address
     * @param APort    TCP port
     *)
    Constructor Create(AHost:String;APort:Word);
    { interface IDeviceCommunicator }
    Procedure Send(St:String); override;
    Function  Receive : String; override;
    Function Query(St: String): String; override;
  End;

Implementation
Uses Errors,CTypes,BaseUnix,Sockets,PasGpibUtils;

{ TTCPLeCroyCommunicator }

Const
  EOI_FLAG     = $01;
  SRQ_FLAG     = $08;
  CLEAR_FLAG   = $10;
  LOCKOUT_FLAG = $20;
  REMOTE_FLAG  = $40;
  DATA_FLAG    = $80;

Type
  PLeCroyHeader = ^TLeCroyHeader;
  TLeCroyHeader = packed record
    Flags    : Byte;
    Reserved : Array[0..2] of Byte;
    Length   : CInt32;
  End;

Constructor TTCPLeCroyCommunicator.Create(AHost: String; APort: Word);
Begin
  inherited Create(AHost,APort);
  FIndex := 0;
End;

Procedure TTCPLeCroyCommunicator.Send(St: String);
Var Header : PLeCroyHeader;
Begin
  // prepend message with header
  St := Space(SizeOf(Header^)) + St;
  Header := @St[1];
  Header^.Flags := DATA_FLAG or EOI_FLAG;
  FillChar(Header^.Reserved,SizeOf(Header^.Reserved),0);
  Header^.Reserved[0] := $01;
  Header^.Reserved[1] := FIndex and $FF + 5;
  Header^.Length := HToNL(Length(St) - SizeOf(Header^));
  FSocket.Write(St[1],Length(St));
  Inc(FIndex);
End;

Function TTCPLeCroyCommunicator.Receive: String;
Var Pos,Len : Integer;
    Waiting : Integer;
    Header  : TLeCroyHeader;
Begin
  SetLength(Result,0);
  Waiting := SelectRead(FSocket.Handle,FTimeout);
  if Waiting = 0 then
    raise Exception.Create('Communication timeout for TCP/IP stream')  // no data -> timeout
  else if Waiting < 0 then
    raise Exception.Create('Error while reading from TCP/IP stream: '+StrError(FpGetErrno));
  Pos := 0;
  repeat
    // receive header
    Len := FSocket.Read(Header,SizeOf(Header));
    if Len <> SizeOf(Header) then
      raise Exception.Create('Error receiving header: '+StrError(FpGetErrno));
    Header.Length := NToHl(Header.Length);
    if Header.Length < 0 then Exit;
    // receive data
    SetLength(Result,Pos+Header.Length);
    repeat
      Len := FSocket.Read(Result[Pos+1],Length(Result) - Pos);
      Pos := Pos + Len;
    Until Pos >= Length(Result);
  Until Header.Flags and EOI_FLAG <> 0;
End;

Function TTCPLeCroyCommunicator.Query(St: String): String;
Begin
  Send(St);
  Result := Receive;
End;

End.

