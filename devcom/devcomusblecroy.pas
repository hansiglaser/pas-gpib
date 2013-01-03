Unit DevComUSBLeCroy;

{$mode objfpc}{$H+}

// use interfaces without IUnknown as ancestor, so they don' have reference
// counting, the classes don't need to be derived from TInterfacedObject, and
// there are no strange problems when destroying objects using our classes
{$interfaces corba}

Interface

Uses
  Classes, SysUtils,
  USBLeCroy, DevCom;

Type
  { TUSBLeCroyCommunicator }

  (**
   * Communicate to a LeCroy WaveJet and WaveAce via USB using their
   * proprietary virtual COM port protocol.
   *
   * The method Send always adds a newline (^M) after the message.
   * The method Query() will trim all whitespace of the received message.
   *)
  TUSBLeCroyCommunicator = class(IDeviceCommunicator)
  private
    FDevice : TUSBLeCroy;
  public
    Constructor Create(ADevice : TUSBLeCroy);
    { interface IDeviceCommunicator }
    Procedure Send(St:String);
    Function  Receive : String;
    Function  Query(St: String): String;
    Procedure SetTimeout(ATimeout:LongInt);   // in us
  End;

Implementation

{ TUSBLeCroyCommunicator }

Constructor TUSBLeCroyCommunicator.Create(ADevice : TUSBLeCroy);
Begin
  FDevice := ADevice;
  SetTimeout(500000{us});  // default to 500ms
End;

Procedure TUSBLeCroyCommunicator.Send(St:String);
Begin
  FDevice.Send(St+^M);
End;

Function TUSBLeCroyCommunicator.Receive : String;
Begin
  Result := FDevice.Recv;
End;

Function TUSBLeCroyCommunicator.Query(St:String) : String;
Begin
  Send(St);
  Result := Trim(Receive);
End;

Procedure TUSBLeCroyCommunicator.SetTimeout(ATimeout:LongInt);
Begin
  FDevice.Timeout := (ATimeout + 500) div 1000;   // round to nearest value in ms
End;

End.

