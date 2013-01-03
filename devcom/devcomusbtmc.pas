Unit DevComUSBTMC;

{$mode objfpc}{$H+}

// use interfaces without IUnknown as ancestor, so they don' have reference
// counting, the classes don't need to be derived from TInterfacedObject, and
// there are no strange problems when destroying objects using our classes
{$interfaces corba}

Interface

Uses
  Classes, SysUtils,
  DevCom, UsbTmc;

Type
  { TUSBTMCCommunicator }

  (**
   * Communicate to a remote instrument via USB using the USB TMC class and
   * USB488 subclass.
   *
   * The method Send always adds a newline (^J) after the message.
   * The method Query() will trim all whitespace of the received message.
   *)
  TUSBTMCCommunicator = class(IDeviceCommunicator)
  private
    FDevice       : TUSBTMCUSB488;
    FTransferSize : Integer;
  public
    Constructor Create(ADevice : TUSBTMCUSB488);
    { interface IDeviceCommunicator }
    Procedure Send(St:String);
    Function  Receive : String;
    Function  Query(St: String): String;
    Procedure SetTimeout(ATimeout:LongInt);   // in us
    { USBTMC functions }
    // (maximum) number of bytes used for DEV_DEP_MSG_IN
    property TransferSize : Integer read FTransferSize write FTransferSize;
  End;

Implementation

{ TUSBTMCCommunicator }

Constructor TUSBTMCCommunicator.Create(ADevice : TUSBTMCUSB488);
Begin
  FDevice := ADevice;
  SetTimeout(500000{us});  // default to 500ms
  FTransferSize := 2048;
End;

Procedure TUSBTMCCommunicator.Send(St:String);
Begin
  FDevice.Send(St+^J);
End;

Function TUSBTMCCommunicator.Receive : String;
Begin
  Result := FDevice.Recv(FTransferSize);
End;

Function TUSBTMCCommunicator.Query(St:String) : String;
Begin
  Send(St);
  Result := Trim(Receive);
End;

Procedure TUSBTMCCommunicator.SetTimeout(ATimeout:LongInt);
Begin
  FDevice.Timeout := (ATimeout + 500) div 1000;   // round to nearest value in ms
End;

End.

