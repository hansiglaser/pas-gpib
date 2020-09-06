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
  TErrorHandler = Procedure;    // raises an exception if an unrecoverable error has happend

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
    FErrorHandler       : TErrorHandler;
    FEnableErrorHandler : Boolean;
  public
    Constructor Create(ADevice : TUSBTMCUSB488);
    { interface IDeviceCommunicator }
    Procedure Send(St:String);
    Function  Receive : String;
    Function  Query(St: String): String;
    Procedure SetTimeout(ATimeout:LongInt);   // in us
    Function  GetTimeout:LongInt;   // in us
    { USBTMC functions }
    Function WaitMessage(MinWaitMs : LongInt) : Byte;
    Procedure CheckErrors(Status : Byte);
    Procedure CheckErrors;
    // (maximum) number of bytes used for DEV_DEP_MSG_IN
    property TransferSize : Integer read FTransferSize write FTransferSize;
    property ErrorHandler : TErrorHandler read FErrorHandler write FErrorHandler;
  End;

Implementation

{ TUSBTMCCommunicator }

Constructor TUSBTMCCommunicator.Create(ADevice : TUSBTMCUSB488);
Begin
  FDevice := ADevice;
  SetTimeout(500000{us});  // default to 500ms
  FTransferSize := 2048;
  FEnableErrorHandler := True;
End;

Procedure TUSBTMCCommunicator.Send(St:String);
Begin
//WriteLn('Send: ''',St,'''');
//ReadLn;
  FDevice.Send(St+^J);
End;

Function TUSBTMCCommunicator.Receive : String;
Var Status : Byte;
Begin
  // wait until a message is available (with FDevice.Timeout, in steps of 10ms)
  Status := WaitMessage(10);
  if (Status and IEEE488_StatusByte_MessageAvailable) = 0 then
    Begin
      // no message available, is there an error?
      CheckErrors(Status);     // will call FErrorHandler, which might raise an exception
      raise Exception.Create('TUSBTMCCommunicator.Receive: Timeout, no message available');
    End;

  // get the message
  Result := FDevice.Recv(FTransferSize);
//WriteLn('Recv: ''',Result,'''');
  // if there was a further error, this has to be checked by the caller, e.g., Query
End;

Function TUSBTMCCommunicator.Query(St:String) : String;
Var Status  : Byte;
    Message : String;
Begin
  // first check if there was an error with a previous command
  Status := FDevice.ReadStatusByte;
  CheckErrors(Status);
  // check if an old message is still waiting
  if (Status and IEEE488_StatusByte_MessageAvailable) <> 0 then
    Begin
      Message := Trim(FDevice.Recv(FTransferSize));
      raise Exception.Create('Error: Before sending '''+St+''', a message from the device was available: '''+Message+'''');
    End;

  // send the command
  Send(St);
  // wait for reply
  Result := Trim(Receive);

  // finally, check if there was an error with the current command
  CheckErrors;   // has its own ReadStatusByte
End;

Procedure TUSBTMCCommunicator.SetTimeout(ATimeout:LongInt);
Begin
  FDevice.Timeout := (ATimeout + 500) div 1000;   // round to nearest value in ms
End;

Function TUSBTMCCommunicator.GetTimeout : LongInt;
Begin
  Result := FDevice.Timeout*1000;
End;

(**
 * Wait until a message is available
 *
 * Every MinWaitMs it is checked, whether a message is available.
 * The total timeout is FDevice.Timeout.
 *)
Function TUSBTMCCommunicator.WaitMessage(MinWaitMs : LongInt) : Byte;
Var Timeout : QWord;
    Status  : Byte;
Begin
  TimeOut := GetTickCount64 + FDevice.Timeout;
  repeat
    Status := FDevice.ReadStatusByte;
    if (Status and IEEE488_StatusByte_MessageAvailable) <> 0 then
      Exit(Status);
    Sleep(MinWaitMs);
  Until GetTickCount64 > Timeout;
  Result := Status;
End;

(**
 * Check if the StatusByte signals an error, then execute the error handler
 *)
Procedure TUSBTMCCommunicator.CheckErrors(Status:Byte);
Var OldEnableErrorHandler : Boolean;
Begin
  if FEnableErrorHandler and assigned(FErrorHandler) and ((Status and IEEE488_StatusByte_ErrorQueue) <> 0) then
    Begin
      try
        OldEnableErrorHandler := FEnableErrorHandler;
        FEnableErrorHandler := False;   // no error checking while error handling
        FErrorHandler;
      finally
        FEnableErrorHandler := OldEnableErrorHandler;
      End;
    End;
End;

Procedure TUSBTMCCommunicator.CheckErrors;
Var Status : Byte;
Begin
  if not (FEnableErrorHandler and assigned(FErrorHandler)) then Exit;
  Status := FDevice.ReadStatusByte;
  CheckErrors(Status);
End;

End.

