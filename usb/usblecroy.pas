Unit USBLeCroy;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LibUsb, LibUsbOop, LibUsbUtil;

Type

  { TUSBLeCroy }

  (**
   * Proprietary LeCroy WaveJet and WaveAce virtual COM port driver
   *)
  TUSBLeCroy = class(TLibUsbDevice)
  protected
    { USB variables }
    FInterface  : TLibUsbInterface;
    FEPIn       : TLibUsbBulkInEndpoint;
    FEPOut      : TLibUsbBulkOutEndpoint;
    FEPInt      : TLibUsbInterruptInEndpoint;
    FTimeout    : Integer;     // in ms
  private
    Procedure VendorMsg(Data:String);
  public
    Constructor Create(AContext:TLibUsbContext;AidVendor,AidProduct:Word;AConfigValue,AIntfNum,AAltSet:Byte);
    Destructor  Destroy; override;
    { USB methods }
    Procedure Send(St:String);
    Function Recv : String;
    property Timeout : Integer read FTimeout write FTimeout;
  End;

Implementation

{ TUSBLeCroy }

Constructor TUSBLeCroy.Create(AContext:TLibUsbContext;AidVendor,AidProduct:Word;AConfigValue,AIntfNum,AAltSet:Byte);
Var IntEP : Plibusb_endpoint_descriptor;
Begin
  Inherited Create(AContext,AidVendor,AidProduct);
  SetConfiguration(AConfigValue);

  // create handlers for the endpoints (and the interface they belong to)
  FInterface := TLibUsbInterface.Create(Self,FindInterface(AIntfNum,AAltSet));
  FEPIn      := TLibUsbBulkInEndpoint.     Create(FInterface,FInterface.FindEndpoint(@MatchEPBulkIn, Nil));
  FEPOut     := TLibUsbBulkOutEndpoint.    Create(FInterface,FInterface.FindEndpoint(@MatchEPBulkOut,Nil));
  FEPInt     := TLibUsbInterruptInEndpoint.Create(FInterface,FInterface.FindEndpoint(@MatchEPIntrIn, Nil));

  FTimeout := 500;    // default to 500ms

  FEPIn. ClearHalt;
  FEPOut.ClearHalt;

  // setup virtual COM port
  VendorMsg(#$03#$01);
  VendorMsg(#$01#$03);
  VendorMsg(#$00#$08#$01);   // $05: 19200, $08: 115200
  VendorMsg(#$04#$00#$00);
  VendorMsg(#$01#$03);
  VendorMsg(#$01#$03);
  VendorMsg(#$00#$08#$01);   // $05: 19200, $08: 115200
  VendorMsg(#$04#$00#$00);
  VendorMsg(#$02#$11#$13);
  VendorMsg(#$04#$00#$00);
  VendorMsg(#$00#$08#$11);   // $05: 19200, $08: 115200
  VendorMsg(#$04#$00#$00);
End;

Destructor TUSBLeCroy.Destroy;
Begin
  if assigned(Control) then
    VendorMsg(#$03#$00);
  FEPInt.Free;
  FEPOut.Free;
  FEPIn.Free;
  FInterface.Free;
  Inherited Destroy;
End;

Procedure TUSBLeCroy.Send(St:String);
Var Result : LongInt;
Begin
  Result := FEPOut.Send(St[1],Length(St),FTimeout);
  if Result = 0 then
    Begin
      WriteLn('Warning: Bulk-OUT endpoint ',FEPOut.Addr,' is halted, retrying.');
      FEPOut.ClearHalt;
      // retry
      Result := FEPOut.Send(St[1],Length(St),FTimeout);
    End;

  if Result < 0 then
    raise Exception.CreateFmt('TUSBLeCroy.Send: USB Error %d: %s',[Result,FContext.ErrorName(Result)])
  else if Result <> Length(St) then
    raise Exception.CreateFmt('TUSBLeCroy.Send: Len = %d should be %d, %s',[Result,Length(St),FContext.ErrorName(Result)]);
End;

Function TUSBLeCroy.Recv : String;
Var MaxLen,Pos,Len : LongInt;
Begin
  MaxLen := 20*1024;
  SetLength(Result,MaxLen);
  // must be large enough for the whole PNG image of the screenshot, otherwise
  // we get a LIBUSB_ERROR_OVERFLOW :-(
  Pos := 1;
  repeat
    // increase buffer if too small
    if MaxLen-Pos+1 < 64 then
      Begin
        MaxLen := MaxLen + 2048;
        SetLength(Result,MaxLen);
      End;
    Len := FEPIn.Recv(Result[Pos],MaxLen-Pos+1,FTimeout);
    if Len > 0 then
      Pos := Pos + Len;
  Until (Len <= 0) or (Pos > MaxLen) or ((Result[Pos-2]=#$0A) and (Result[Pos-1]=#$0D));
  // The above exit condition and the condition to raise an exception below are
  // a dirty trick! The loop is stopped either if an error happened (Len < 0),
  // no new data is there (Len = 0), or the received data ends with $0A $0D.
  // This is necessary, because this newline is sent in an extra USB IN packet.
  // When retrieving a screenshot, the data doesn't end in $0A $0D, so we have
  // to wait for the timeout, because here we don't know anything about the
  // meaning of the data, and therefore are not allowed to interpret the
  // special header stating the length of the binary data.
  if (Pos = 1) and (Len < 0) then
    raise Exception.CreateFmt('TUSBLeCroy.Recv: USB Error %d: %s',[Len,FContext.ErrorName(Len)]);
  SetLength(Result,Pos-1);
End;

Procedure TUSBLeCroy.VendorMsg(Data:String);
Var Len : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_OUT or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } $0B,   // all observed requests use this number
    { wValue        } 0,
    { wIndex        } 0,
    { Buf           } Data[1],
    { wLength       } Length(Data),
    { Timeout       } FTimeout);
  if Len <> Length(Data) then
    raise Exception.CreateFmt('VendorMsg: Len = %d should be %d, %s',[Len,Length(Data),FContext.ErrorName(Len)]);
End;

End.

(*
USB Descriptor:

ID 05ff:0021 LeCroy Corp.
Device Descriptor:
  bLength                18
  bDescriptorType         1
  bcdUSB               1.10
  bDeviceClass            0 (Defined at Interface level)
  bDeviceSubClass         0
  bDeviceProtocol         0
  bMaxPacketSize0         8
  idVendor           0x05ff LeCroy Corp.
  idProduct          0x0021
  bcdDevice            0.02
  iManufacturer           0
  iProduct                0
  iSerial                 0
  bNumConfigurations      1
  Configuration Descriptor:
    bLength                 9
    bDescriptorType         2
    wTotalLength           39
    bNumInterfaces          1
    bConfigurationValue     1
    iConfiguration          0
    bmAttributes         0xc0
      Self Powered
    MaxPower              100mA
    Interface Descriptor:
      bLength                 9
      bDescriptorType         4
      bInterfaceNumber        0
      bAlternateSetting       0
      bNumEndpoints           3
      bInterfaceClass         0 (Defined at Interface level)
      bInterfaceSubClass      0
      bInterfaceProtocol      0
      iInterface              0
      Endpoint Descriptor:
        bLength                 7
        bDescriptorType         5
        bEndpointAddress     0x01  EP 1 OUT
        bmAttributes            2
          Transfer Type            Bulk
          Synch Type               None
          Usage Type               Data
        wMaxPacketSize     0x0040  1x 64 bytes
        bInterval               0
      Endpoint Descriptor:
        bLength                 7
        bDescriptorType         5
        bEndpointAddress     0x82  EP 2 IN
        bmAttributes            2
          Transfer Type            Bulk
          Synch Type               None
          Usage Type               Data
        wMaxPacketSize     0x0040  1x 64 bytes
        bInterval               0
      Endpoint Descriptor:
        bLength                 7
        bDescriptorType         5
        bEndpointAddress     0x83  EP 3 IN
        bmAttributes            3
          Transfer Type            Interrupt
          Synch Type               None
          Usage Type               Data
        wMaxPacketSize     0x0008  1x 8 bytes
        bInterval              10


USB Protocol Reverse Engineering:
 - Windows virtual COM port driver
 - HyperTerm
 - simply type "*IDN?", "DATE?", ... and the result is returned
 - HHD Software USB Monitor reveals
    - at connecting with HyperTerm at 19200,n,8,1 -> 115200,n,8,1 -> 19200,n,8,1:
       - DOWN Vendor Device: Dest: Device, Req: 0x0B, Value: 0x00, 2 bytes: 0x03 0x01
       - UP   Control Transfer: 40 0B 00 00 00 00 02 00   -> 03 01 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 2)
       - DOWN Vendor Device: Dest: Device, Req: 0x0B, Value: 0x00, 2 bytes: 0x01 0x03
       - UP   Control Transfer: 40 0B 00 00 00 00 02 00   -> 01 03 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 2)
       - UP   0 bytes from EP3IN (Intr)
       - DOWN Abort Pipe EP2IN (Bulk)
       - UP   0 bytes from EP2IN (Bulk)
       - UP   Abort Pipe EP2IN (Bulk)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 00 05 01 / 00 08 11 / 00 05 11 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 04 00 00 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 02 00   -> 01 03 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 2)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 02 00   -> 01 03 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 2)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 00 05 01 / 00 08 11 / 00 05 11 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 04 00 00 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - UP   0 bytes from EP3IN (Intr)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 02 11 13 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 04 00 00 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 00 05 11 / 00 08 11 / 00 05 11 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
       - DOWN Vendor Device: ...
       - UP   Control Transfer: 40 0B 00 00 00 00 03 00   -> 04 00 00 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 3)
    - during open connection
       - permanent incoming 0-size packets through EP3IN (interrupt)
       - typing sends each character in a dedicated EP1OUT (bulk) 1-byte sized packet
       - newline is coded as $0D
       - after that, two packets come back, one with the reply data, and one
         with $0A $0D
    - closing the connection
       - many abort pipe, sync reset pipe and clear stall, ...
       - DOWN Vendor Device: Dest: Device, Req: 0x0B, Value: 0x00, 2 bytes: 0x03 0x01
       - UP   Control Transfer: 40 0B 00 00 00 00 02 00   -> 03 01 ?? 03 00 (Rcpt: Device, Req.Type: Vendor, Dir: Host->Dev, Req: 0x0B, Value: 0, Index: 0, Length: 2)

Playing with "eztool":
  $ ./eztool
  > connect -user 05ff:0021
  > claim 0 0
  Interface 0 (Alternate 0) "?":
    EP  1 OUT Bulk
    EP  2 IN  Bulk
    EP  3 IN  Interrupt
  > bulkout 1 0x2a 0x49 0x44 0x4e 0x3f 0x0d
  0000: 2A 49 44 4E 3F 0D   *IDN?.
  Sent 6 bytes
  > bulkin 2 100
  Received 33 bytes
  0000: 4C 45 43 52 4F 59 2C 57 4A 33 32 34 41 2C 4C 43   LECROY,WJ324A,LC
  0010: 52 59 30 31 30 32 4A 32 31 38 36 38 2C 35 2E 30   RY0102J21868,5.0
  0020: 36                                                6
  > bulkin 2 100
  Received 2 bytes
  0000: 0A 0D   ..
  > bulkout 1 0x44 0x41 0x54 0x45 0x3f 0x0d
  0000: 44 41 54 45 3F 0D   DATE?.
  Sent 6 bytes
  > bulkin 2 100
  Received 19 bytes
  0000: 33 30 2C 44 45 43 2C 32 30 31 32 2C 31 39 2C 38   30,DEC,2012,19,8
  0010: 2C 31 37                                          ,17
  > bulkin 2 100
  Received 2 bytes
  0000: 0A 0D   ..

*)

