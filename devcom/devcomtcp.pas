Unit DevComTCP;

{$mode objfpc}{$H+}

// use interfaces without IUnknown as ancestor, so they don' have reference
// counting, the classes don't need to be derived from TInterfacedObject, and
// there are no strange problems when destroying objects using our classes
{$interfaces corba}

Interface

Uses
  Classes, SysUtils,
  DevCom, SSockets;

Type

  { TTCPCommunicator }

  (**
   * Communicate to a remote instrument via TCP/IP
   *
   * The method Send does not add a newline (^J) after the message.
   * The method Query() will trim all whitespace of the received message.
   *)
  TTCPCommunicator = class(IDeviceCommunicator)
  protected
    FSocket : TInetSocket;
    FTimeout : LongInt;
  public
    (**
     * Instanciate and open TCP/IP connection
     *
     * @param AHost    host name or IP address
     * @param APort    TCP port
     *)
    Constructor Create(AHost:String;APort:Word);
    Destructor  Destroy; override;
    { interface IDeviceCommunicator }
    Procedure Send(St:String); virtual;
    Function  Receive : String; virtual;
    Function  Query(St:String):String; virtual;
    Procedure SetTimeout(ATimeout:LongInt);   // in us
  End;

Implementation
Uses Errors, BaseUnix, PasGpibUtils;

{ TTCPCommunicator }

Constructor TTCPCommunicator.Create(AHost: String; APort: Word);
Begin
  FSocket := TInetSocket.Create(AHost,APort);
  FTimeout := 1000000; // default to 1000ms, don't set to 100ms, this is not enough!
End;

Destructor TTCPCommunicator.Destroy;
Begin
  FSocket.Free;
  Inherited Destroy;
End;

Procedure TTCPCommunicator.Send(St: String);
Begin
  St := St + ^J;
  FSocket.Write(St[1],Length(St));
End;

Function TTCPCommunicator.Receive: String;
Var Pos,Len : Integer;
    Waiting : Integer;
Begin
  Pos := 1;
  Len := 0;
  SetLength(Result,0);
  repeat
    Waiting := SelectRead(FSocket.Handle,FTimeout);
    if Waiting = 0 then
      raise Exception.Create('Communication timeout for TCP/IP stream')  // no data -> timeout
    else if Waiting < 0 then
      raise Exception.Create('Error while reading from TCP/IP stream: '+StrError(FpGetErrno));
    SetLength(Result,Pos-1+1024);
    Len := FSocket.Read(Result[Pos],1024);
    if Result[Pos+Len-1] = ^J then Break;
    Pos := Pos + Len;
  Until Len = 0;
  SetLength(Result,Pos+Len-1);
End;

Function TTCPCommunicator.Query(St: String): String;
Begin
  Send(St);
  Result := Trim(Receive);
End;

Procedure TTCPCommunicator.SetTimeout(ATimeout: LongInt);
Begin
  FTimeout := ATimeout;
End;

End.

