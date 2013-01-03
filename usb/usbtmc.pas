Unit UsbTmc;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LibUsb, LibUsbOop, LibUsbUtil;

Type
  TUSBTMCIntfInfo = record
    Device         : Plibusb_device;
    DevDescr       : libusb_device_descriptor;
    ConfigDescr    : libusb_config_descriptor;
    IntfDescr      : libusb_interface_descriptor;
  End;
  TUSBTMCIntfInfoStrings = record
    sVendor        : String;
    sProduct       : String;
    sSerial        : String;
    sConfiguration : String;
    sInterface     : String;
  End;
  TUSBTMCIntfInfos = Array of TUSBTMCIntfInfo;
  TUSBTMCSubClass = (tcsNone,tcsUSB488);
  TUSBTMCDevDepMsgOut = packed record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved1    : Byte;
    TransferSize : Cardinal;
    Attributes   : Byte;
    Reserved2    : Array[0..2] of Byte;
    Data         : Array[0..0] of Byte;
  End;
  TUSBTMCRequestDevDepMsgIn = packed record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved1    : Byte;
    TransferSize : Cardinal;
    Attributes   : Byte;
    TermChar     : Char;
    Reserved2    : Array[0..1] of Byte;
  End;
  TUSBTMCDevDepMsgIn = packed record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved1    : Byte;
    TransferSize : Cardinal;
    Attributes   : Byte;
    Reserved2    : Array[0..2] of Byte;
    Data         : Array[0..0] of Byte;
  End;
  TUSBTMCVendorMsgOut = packed record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved1    : Byte;
    TransferSize : Cardinal;
    Reserved2    : Array[0..3] of Byte;
    Data         : Array[0..0] of Byte;
  End;
  TUSBTMCRequestVendorMsgIn = packed record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved1    : Byte;
    TransferSize : Cardinal;
    Reserved2    : Array[0..3] of Byte;
  End;
  TUSBTMCVendorMsgIn = record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved1    : Byte;
    TransferSize : Cardinal;
    Reserved2    : Array[0..3] of Byte;
    Data         : Array[0..0] of Byte;
  End;
  TUSBTMCInitiateAbortBulkOut = record
    Status : Byte;
    Tag    : Byte;
  End;
  TUSBTMCCheckAbortBulkOutStatus = record
    Status   : Byte;
    Reserved : Array[0..2] of Byte;
    BytesRx  : Cardinal;
  End;
  TUSBTMCInitiateAbortBulkIn = record
    Status : Byte;
    Tag    : Byte;
  End;
  TUSBTMCCheckAbortBulkInStatus = record
    Status   : Byte;
    Bits     : Byte;
    Reserved : Array[0..1] of Byte;
    BytesTx  : Cardinal;
  End;
  TUSBTMCCheckClearStatus = packed record
    Status   : Byte;
    Bits     : Byte;
  End;
  TUSBTMCGetCapabilities = packed record
    Status     : Byte;
    Reserved1  : Byte;
    BCDVersion : Word;
    IntfCap    : Byte;
    DevCap     : Byte;
    Reserved2  : Array[0.. 5] of Byte;
    Reserved3  : Array[0..11] of Byte;
  End;
  TUSBTMCUSB488GetCapabilities = packed record  // overlay for TUSBTMCGetCapabilities
    Status     : Byte;
    Reserved1  : Byte;
    BCDVersion : Word;
    IntfCap    : Byte;
    DevCap     : Byte;
    Reserved2  : Array[0.. 5] of Byte;
    BCDUSB488     : Word;
    Usb488IntfCap : Byte;
    Usb488DevCap  : Byte;
    Reserved3  : Array[0..7] of Byte;
  End;

  { TUSBTMCBase }

  TUSBTMCBase = class(TLibUsbDevice)
  Private
    Function GetCapIndicatorPulse: Boolean;
    Function GetCapListenOnly: Boolean;
    Function GetCapTalkOnly: Boolean;
    Function GetCapTermChar: Boolean;
  protected
    FDescriptor : TUSBTMCIntfInfo;
    FSubClass   : TUSBTMCSubClass;
    { USB variables }
    FInterface  : TLibUsbInterface;
    FEPIn       : TLibUsbBulkInEndpoint;
    FEPOut      : TLibUsbBulkOutEndpoint;
    FEPInt      : TLibUsbInterruptInEndpoint;
    FTimeout    : Integer;
    FLastTag    : Byte;
    FCapabilities : TUSBTMCGetCapabilities;
    Function NextTag : Byte;
  public
    Constructor Create(AContext:TLibUsbContext;ADescriptor:TUSBTMCIntfInfo);
    Constructor Create(AContext:TLibUsbContext;ADevice:Plibusb_device); override;
    Destructor  Destroy; override;
    { USBTMC methods }
    Procedure DevDepMsgOut      (Size:Cardinal;Attributes:Byte;Const Data);   // MsgID = 1, Tag = AutoInc
    Procedure RequestDevDepMsgIn(Size:Cardinal;Attributes:Byte;TermChar:Char);
    Procedure DevDepMsgIn       (Var Size:Cardinal;Var Attributes:Byte;Var Data);
    Procedure VendorSpecificOut (Size:Cardinal;Const Data);
    Procedure RequestVendorMsgIn(Size:Cardinal);
    Procedure VendorMsgIn       (Var Size:Cardinal;Var Data);

    Function InitiateAbortBulkOut    (Var Tag:Byte)                                : Byte;
    Function CheckAbortBulkOutStatus (Var BytesRx:Cardinal)                        : Byte;
    Function InitiateAbortBulkIn     (Var Tag:Byte)                                : Byte;
    Function CheckAbortBulkInStatus  (Var Bits:Byte;Var BytesTx:Cardinal)          : Byte;
    Function InitiateClear                                                         : Byte;
    Function CheckClearStatus        (Var Bits:Byte)                               : Byte;
    Function GetCapabilities                                                       : Byte;
    property UsbTmcBcdVersion  : Word    read FCapabilities.BCDVersion;
    property CapIndicatorPulse : Boolean read GetCapIndicatorPulse;
    property CapTalkOnly       : Boolean read GetCapTalkOnly;
    property CapListenOnly     : Boolean read GetCapListenOnly;
    property CapTermChar       : Boolean read GetCapTermChar;
    property Timeout           : Integer read FTimeout write FTimeout;
    Function IndicatorPulse                                                        : Byte;
    { class methods }
    class Function FindUSBTMCInterfaces(Dev:Plibusb_device;Var Infos:TUSBTMCIntfInfos) : Boolean;
    class Function Scan(AContext:TLibUsbContext) : TUSBTMCIntfInfos;
    class Function GetInfoStrings(AContext:TLibUsbContext;ConstRef IntfInfo:TUSBTMCIntfInfo) : TUSBTMCIntfInfoStrings;
  End;

  { TUSBTMCUSB488 }

  TUSBTMCUSB488 = class(TUSBTMCBase)
  private
    Function GetCapDT1: Boolean;
    Function GetCapRenControl: Boolean;
    Function GetCapRL1: Boolean;
    Function GetCapScpi: Boolean;
    Function GetCapSR1: Boolean;
    Function GetCapTrigger: Boolean;
    Function GetCapUSB4882: Boolean;
    Function GetUsb488BcdVersion: Word;
  public
    Constructor Create(AContext:TLibUsbContext;ADescriptor:TUSBTMCIntfInfo);
    Procedure Trigger;
    Procedure Send(St:String);
    Function Recv(MaxLen:Cardinal) : String;
    property Usb488BcdVersion  : Word    read GetUsb488BcdVersion; // TUSBTMCUSB488GetCapabilities(FCapabilities).BCDUSB488;
    property CapUSB4882     : Boolean read GetCapUSB4882;
    property CapRenControl  : Boolean read GetCapRenControl;
    property CapTrigger     : Boolean read GetCapTrigger;
    property CapScpi        : Boolean read GetCapScpi;
    property CapSR1         : Boolean read GetCapSR1;
    property CapRL1         : Boolean read GetCapRL1;
    property CapDT1         : Boolean read GetCapDT1;
  End;

Implementation

Const
  UsbTmcMsgID_DevDepMsgOut              = 1;
  UsbTmcMsgID_DevDepMsgIn               = 2;
  UsbTmcMsgID_VendorMsgOut              = 126;
  UsbTmcMsgID_VendorMsgIn               = 127;
  UsbTmcRequest_InitiateAbortBulkOut    = 1;
  UsbTmcRequest_CheckAbortBulkOutStatus = 2;
  UsbTmcRequest_InitiateAbortBulkIn     = 3;
  UsbTmcRequest_CheckAbortBulkInStatus  = 4;
  UsbTmcRequest_InitiateClear           = 5;
  UsbTmcRequest_CheckClearStatus        = 6;
  UsbTmcRequest_GetCapabilities         = 7;
  UsbTmcRequest_IndicatorPulse          = 64;
  UsbTmcStatus_Success                  = 1;
  UsbTmcStatus_Pending                  = 2;
  UsbTmcStatus_Failed                   = $80;
  UsbTmcStatus_TransferNotInProgress    = $81;
  UsbTmcStatus_SplitNotInProgress       = $82;
  UsbTmcStatus_SplitInProgress          = $83;
  UsbTmcTransferAttribute_EOM           = $01;
  { USBTMC USB488 sub class }
  UsbTmc488MsgID_Trigger                = 128;
  UsbTmc488Request_ReadStatusByte       = 128;
  UsbTmc488Request_RenControl           = 160;
  UsbTmc488Request_GoToLocal            = 161;
  UsbTmc488Request_LocalLockout         = 162;
  UsbTmc488Status_InterruptInBusy       = $20;
  USBTMC_CAP_INTF_INDICATOR_PULS        = $04;
  USBTMC_CAP_INTF_TALK_ONLY             = $02;
  USBTMC_CAP_INTF_LISTEN_ONLY           = $01;
  USBTMC_CAP_DEV_TERM_CHAR              = $01;
  USB488_CAP_INTF_4882                  = $04;
  USB488_CAP_INTF_REN_CONTROL           = $02;
  USB488_CAP_INTF_TRIGGER               = $01;
  USB488_CAP_DEV_SCPI                   = $08;
  USB488_CAP_DEV_SR1                    = $04;
  USB488_CAP_DEV_RL1                    = $02;
  USB488_CAP_DEV_DT1                    = $01;

{ TUSBTMCBase }

Constructor TUSBTMCBase.Create(AContext:TLibUsbContext;ADescriptor:TUSBTMCIntfInfo);
Var IntEP : Plibusb_endpoint_descriptor;
Begin
  FDescriptor := ADescriptor;
  Inherited Create(AContext,FDescriptor.Device);
  // Set configuration only if we need a different one. If the device (more
  // precisely: the USB-TMC interface) is already claimed by the "usbtmc"
  // kernel driver, SetConfiguration would give a LIBUSB_ERROR_BUSY error.
  // Therefore we don't use SetConfiguration (because its already the correct
  // one), and later in TLibUsbInterface.Create the kernel driver is
  // detached.
  if GetConfiguration <> ADescriptor.ConfigDescr.bConfigurationValue then
    SetConfiguration(ADescriptor.ConfigDescr.bConfigurationValue);

  // create handlers for the endpoints (and the interface they belong to)
  FInterface := TLibUsbInterface.Create(Self,FindInterface(FDescriptor.IntfDescr.bInterfaceNumber,FDescriptor.IntfDescr.bAlternateSetting));
  FEPIn      := TLibUsbBulkInEndpoint.     Create(FInterface,FInterface.FindEndpoint(@MatchEPBulkIn, Nil));
  FEPOut     := TLibUsbBulkOutEndpoint.    Create(FInterface,FInterface.FindEndpoint(@MatchEPBulkOut,Nil));
  // Check for optional interrupt IN endpoint. This is required for 488.2 interfaces and if the SR1
  // capability is supported (see [USB488] p. 3 and p. 9).
  IntEP := FInterface.FindEndpoint(@MatchEPIntrIn, Nil);
  if IntEP <> Nil then
    FEPInt   := TLibUsbInterruptInEndpoint.Create(FInterface,IntEP);

  FTimeout := 500;    // default to 500ms

  { bInterfaceProtocol: 0 ... USBTMC (no subclass), 1 ... USBTMC (USB488) }
  Case FInterface.Descriptor^.bInterfaceProtocol of
    0 : FSubClass := tcsNone;
    1 : FSubClass := tcsUSB488;
  else
    WriteLn('Error: Unknown interface protocol ',FInterface.Descriptor^.bInterfaceProtocol);
  End;

  FEPIn. ClearHalt;
  FEPOut.ClearHalt;

(*  Status := InitiateClear;
  if Status <> UsbTmcStatus_Success then
    raise Exception.CreateFmt('Couldn''t initiate clear (Status = %d)',[Status]);
  For I := 0 to 10 do
    Begin
      Status := CheckClearStatus(Bits);
      if Status <> UsbTmcStatus_Pending then break;
      Sleep(50);
    End;
  if Status <> UsbTmcStatus_Success then
    raise Exception.CreateFmt('Couldn''t finish clear (Status = %d)',[Status]);*)

  if GetCapabilities <> UsbTmcStatus_Success then
    raise Exception.Create('Error in GetCapabilities');
  FLastTag := 12;
End;

Constructor TUSBTMCBase.Create(AContext : TLibUsbContext; ADevice : Plibusb_device);
Var Infos : TUSBTMCIntfInfos;
Begin
  SetLength(Infos,0);
  if not TUSBTMCBase.FindUSBTMCInterfaces(ADevice,Infos) then
    raise Exception.Create('The device doesn''t have a USB-TMC interface.');
  if Length(Infos) > 1 then
    WriteLn('Warning: The device has ',Length(Infos),' USB-TMC interfaces.',
      ' Using the first with bInterfaceNumber = ',Infos[0].IntfDescr.bInterfaceNumber,
      ' and bAlternateSetting = ',Infos[0].IntfDescr.bAlternateSetting);
  Create(AContext,Infos[0]);
End;

Destructor TUSBTMCBase.Destroy;
Begin
  FEPInt.Free;
  FEPOut.Free;
  FEPIn.Free;
  FInterface.Free;
  Inherited Destroy;
End;

Procedure TUSBTMCBase.DevDepMsgOut(Size:Cardinal;Attributes:Byte;Const Data);
Var Packet : ^TUSBTMCDevDepMsgOut;
    Len    : LongInt;
    Result : LongInt;
Begin
  Len := SizeOf(TUSBTMCDevDepMsgOut)-1 + (Size + 3) and $FFFFFFFC;
  Packet := GetMem(Len);
  FillChar(Packet^,Len,0);
  Packet^.MsgID        := UsbTmcMsgID_DevDepMsgOut;
  Packet^.Tag          := NextTag;
  Packet^.TagInverse   := not Packet^.Tag;
  Packet^.TransferSize := Size;
  Packet^.Attributes   := Attributes;
  Move(Data,Packet^.Data,Size);

//  WriteLn('DevDepMsgOut');
//  Dump(Packet^,Len);

  Result := FEPOut.Send(Packet^,Len,FTimeout);
  if Result = 0 then
    Begin
      WriteLn('Warning: Bulk-OUT endpoint ',FEPOut.Addr,' is halted, retrying.');
      FEPOut.ClearHalt;
      // retry
      Result := FEPOut.Send(Packet^,Len,FTimeout);
    End;

  FreeMem(Packet);
  if Result <> Len then
    raise Exception.CreateFmt('DevDepMsgOut: Len = %d should be %d, %s',[Result,Len,FContext.ErrorName(Result)]);
End;

(**
 * Send a request for a device dependent message IN.
 *
 * [USB-TMC] p. 5f, p. 8f, [USB488] p. 7
 *)
Procedure TUSBTMCBase.RequestDevDepMsgIn(Size: Cardinal;Attributes:Byte; TermChar:Char);
Var Packet : TUSBTMCRequestDevDepMsgIn;
    Result : LongInt;
Begin
  FillChar(Packet,SizeOf(Packet),0);
  Packet.MsgID        := UsbTmcMsgID_DevDepMsgIn;
  Packet.Tag          := NextTag;
  Packet.TagInverse   := not Packet.Tag;
  Packet.TransferSize := Size;
  Packet.Attributes   := Attributes;
  Packet.TermChar     := TermChar;

//  WriteLn('RequestDevDepMsgIn');
//  Dump(Packet,SizeOf(Packet));

  Result := FEPOut.Send(Packet,SizeOf(Packet),FTimeout);
  if Result <> SizeOf(Packet) then
    raise Exception.CreateFmt('RequestDevDepMsgIn: Len = %d should be %d, %s',[Result,SizeOf(Packet),FContext.ErrorName(Result)]);
End;

(**
 * Receive a device dependent message IN.
 *
 * [USB-TMC] p. 5f, p. 13f, [USB488] p. 7
 *)
Procedure TUSBTMCBase.DevDepMsgIn(Var Size:Cardinal;Var Attributes:Byte; Var Data);
Var Packet : ^TUSBTMCDevDepMsgIn;
    Len    : LongInt;
    Result : LongInt;
Begin
  Len := SizeOf(TUSBTMCDevDepMsgIn)-1 + (Size + 3) and $FFFFFFFC;
  Packet := GetMem(Len);
  try
    Result := FEPIn.Recv(Packet^,Len,FTimeout);
    if Packet^.MsgID <> UsbTmcMsgID_DevDepMsgIn then
      raise Exception.CreateFmt('Invalid MsgID (%d)',[Packet^.MsgID]);
    if Packet^.Tag <> not Packet^.TagInverse then
      raise Exception.CreateFmt('Tag (%d) and TagInvers (%d) don''t match',[Packet^.Tag,Packet^.TagInverse]);
    if Packet^.Tag <> FLastTag then
      raise Exception.CreateFmt('Tag (%d) doesn''t match last tag (%d)',[Packet^.Tag,FLastTag]);
    if Packet^.TransferSize > Size then
      raise Exception.CreateFmt('TransferSize (%d) > Size (%d)',[Packet^.TransferSize,Size]);

//  WriteLn('DevDepMsgIn');
//  Len := SizeOf(TUSBTMCDevDepMsgIn)-1 + (Packet^.TransferSize + 3) and $FFFFFFFC;
//  Dump(Packet^,Len);

    Size       := Packet^.TransferSize;
    Attributes := Packet^.Attributes;
    Move(Packet^.Data,Data,Size);
  finally
    FreeMem(Packet);
  End;
End;

Procedure TUSBTMCBase.VendorSpecificOut(Size:Cardinal; Const Data);
Var Packet : ^TUSBTMCVendorMsgOut;
    Len    : LongInt;
    Result : LongInt;
Begin
  Len := SizeOf(TUSBTMCVendorMsgOut)-1 + (Size + 3) and $FFFFFFFC;
  Packet := GetMem(Len);
  FillChar(Packet^,SizeOf(TUSBTMCVendorMsgOut),0);
  Packet^.MsgID        := UsbTmcMsgID_VendorMsgOut;
  Packet^.Tag          := NextTag;
  Packet^.TagInverse   := not Packet^.Tag;
  Packet^.TransferSize := Size;
  Move(Data,Packet^.Data,Size);
  Result := FEPOut.Send(Packet^,Len,FTimeout);
  FreeMem(Packet);
  if Result <> Len then
    raise Exception.CreateFmt('VendorSpecificOut: Len = %d should be %d, %s',[Result,Len,FContext.ErrorName(Result)]);
End;

Procedure TUSBTMCBase.RequestVendorMsgIn(Size:Cardinal);
Var Packet : TUSBTMCRequestVendorMsgIn;
    Result : LongInt;
Begin
  FillChar(Packet,SizeOf(Packet),0);
  Packet.MsgID        := UsbTmcMsgID_VendorMsgIn;
  Packet.Tag          := NextTag;
  Packet.TagInverse   := not Packet.Tag;
  Packet.TransferSize := Size;
  Result := FEPOut.Send(Packet,SizeOf(Packet),FTimeout);
  if Result <> SizeOf(Packet) then
    raise Exception.CreateFmt('RequestVendorMsgIn: Len = %d should be %d, %s',[Result,SizeOf(Packet),FContext.ErrorName(Result)]);
End;

Procedure TUSBTMCBase.VendorMsgIn(Var Size:Cardinal; Var Data);
Var Packet : ^TUSBTMCVendorMsgIn;
    Len    : LongInt;
    Result : LongInt;
Begin
  Len := SizeOf(TUSBTMCVendorMsgIn)-1 + (Size + 3) and $FFFFFFFC;
  Packet := GetMem(Len);
  try
    Result := FEPIn.Recv(Packet^,Len,FTimeout);
    if Packet^.MsgID <> UsbTmcMsgID_VendorMsgIn then
      raise Exception.CreateFmt('Invalid MsgID (%d)',[Packet^.MsgID]);
    if Packet^.Tag <> not Packet^.TagInverse then
      raise Exception.CreateFmt('Tag (%d) and TagInvers (%d) don''t match',[Packet^.Tag,Packet^.TagInverse]);
    if Packet^.Tag <> FLastTag then
      raise Exception.CreateFmt('Tag (%d) doesn''t match last tag (%d)',[Packet^.Tag,FLastTag]);
    if Packet^.TransferSize > Size then
      raise Exception.CreateFmt('TransferSize (%d) > Size (%d)',[Packet^.TransferSize,Size]);
    Size       := Packet^.TransferSize;
    Move(Packet^.Data,Data,Size);
  finally
    FreeMem(Packet);
  End;
End;

Function TUSBTMCBase.InitiateAbortBulkOut(Var Tag:Byte) : Byte;
Var Reply : TUSBTMCInitiateAbortBulkOut;
    Len   : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_ENDPOINT,
    { bRequest      } UsbTmcRequest_InitiateAbortBulkOut,
    { wValue        } Tag,
    { wIndex        } FEPOut.Addr,
    { Buf           } Reply,
    { wLength       } SizeOf(Reply),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Reply) then
    raise Exception.CreateFmt('InitiateAbortBulkOut: Len = %d should be %d, %s',[Len,SizeOf(Reply),FContext.ErrorName(Len)]);
  Result := Reply.Status;
  if Reply.Status <> UsbTmcStatus_Success then
    Exit;
  Tag := Reply.Tag;
End;

Function TUSBTMCBase.CheckAbortBulkOutStatus(Var BytesRx:Cardinal) : Byte;
Var Reply : TUSBTMCCheckAbortBulkOutStatus;
    Len   : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_ENDPOINT,
    { bRequest      } UsbTmcRequest_CheckAbortBulkOutStatus,
    { wValue        } 0,
    { wIndex        } FEPOut.Addr,
    { Buf           } Reply,
    { wLength       } SizeOf(Reply),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Reply) then
    raise Exception.CreateFmt('CheckAbortBulkOutStatus: Len = %d should be %d, %s',[Len,SizeOf(Reply),FContext.ErrorName(Len)]);
  Result := Reply.Status;
  if Reply.Status <> UsbTmcStatus_Success then
    Exit;
  BytesRx := Reply.BytesRx;
End;

Function TUSBTMCBase.InitiateAbortBulkIn(Var Tag:Byte) : Byte;
Var Reply : TUSBTMCInitiateAbortBulkIn;
    Len   : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_ENDPOINT,
    { bRequest      } UsbTmcRequest_InitiateAbortBulkIn,
    { wValue        } Tag,
    { wIndex        } FEPIn.Addr,
    { Buf           } Reply,
    { wLength       } SizeOf(Reply),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Reply) then
    raise Exception.CreateFmt('InitiateAbortBulkIn: Len = %d should be %d, %s',[Len,SizeOf(Reply),FContext.ErrorName(Len)]);
  Result := Reply.Status;
  if Reply.Status <> UsbTmcStatus_Success then
    Exit;
  Tag := Reply.Tag;
End;

Function TUSBTMCBase.CheckAbortBulkInStatus(Var Bits:Byte;Var BytesTx:Cardinal) : Byte;
Var Reply : TUSBTMCCheckAbortBulkInStatus;
    Len   : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_ENDPOINT,
    { bRequest      } UsbTmcRequest_CheckAbortBulkInStatus,
    { wValue        } 0,
    { wIndex        } FEPIn.Addr,
    { Buf           } Reply,
    { wLength       } SizeOf(Reply),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Reply) then
    raise Exception.CreateFmt('CheckAbortBulkInStatus: Len = %d should be %d, %s',[Len,SizeOf(Reply),FContext.ErrorName(Len)]);
  Result := Reply.Status;
  if Reply.Status <> UsbTmcStatus_Success then
    Exit;
  Bits    := Reply.Bits;
  BytesTx := Reply.BytesTx;
End;

Function TUSBTMCBase.InitiateClear : Byte;
Var Status : Byte;
    Len    : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE,
    { bRequest      } UsbTmcRequest_InitiateClear,
    { wValue        } 0,
    { wIndex        } FInterface.Descriptor^.bInterfaceNumber,
    { Buf           } Status,
    { wLength       } SizeOf(Status),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Status) then
    raise Exception.CreateFmt('InitiateClear: Len = %d should be %d, %s',[Len,SizeOf(Status),FContext.ErrorName(Len)]);
  Result := Status;
End;

Function TUSBTMCBase.CheckClearStatus(Var Bits:Byte) : Byte;
Var Reply : TUSBTMCCheckClearStatus;
    Len   : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE,
    { bRequest      } UsbTmcRequest_CheckClearStatus,
    { wValue        } 0,
    { wIndex        } FInterface.Descriptor^.bInterfaceNumber,
    { Buf           } Reply,
    { wLength       } SizeOf(Reply),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Reply) then
    raise Exception.CreateFmt('CheckClearStatus: Len = %d should be %d, %s',[Len,SizeOf(Reply),FContext.ErrorName(Len)]);
  Result := Reply.Status;
  if Reply.Status <> UsbTmcStatus_Success then
    Exit;
  Bits := Reply.Bits;
End;

Function TUSBTMCBase.GetCapabilities : Byte;
Var Len   : LongInt;
Begin
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE,
    { bRequest      } UsbTmcRequest_GetCapabilities,
    { wValue        } 0,
    { wIndex        } FInterface.Descriptor^.bInterfaceNumber,
    { Buf           } FCapabilities,
    { wLength       } SizeOf(FCapabilities),
    { Timeout       } FTimeout);
  if Len <> SizeOf(FCapabilities) then
    raise Exception.CreateFmt('GetCapabilities: Len = %d should be %d, %s',[Len,SizeOf(FCapabilities),FContext.ErrorName(Len)]);
  Result := FCapabilities.Status;
  if Result <> UsbTmcStatus_Success then
    Exit;
End;

Function TUSBTMCBase.IndicatorPulse : Byte;
Var Status : Byte;
    Len    : LongInt;
Begin
  if not CapIndicatorPulse then
    Begin
      WriteLn('Warning: Indicator pulse not supported.');
      Exit(UsbTmcStatus_Failed);
    End;
  Len := Control.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE,
    { bRequest      } UsbTmcRequest_IndicatorPulse,
    { wValue        } 0,
    { wIndex        } FInterface.Descriptor^.bInterfaceNumber,
    { Buf           } Status,
    { wLength       } SizeOf(Status),
    { Timeout       } FTimeout);
  if Len <> SizeOf(Status) then
    raise Exception.CreateFmt('IndicatorPulse: Len = %d should be %d, %s',[Len,SizeOf(Status),FContext.ErrorName(Len)]);
  Result := Status;
End;

{ class methods }

class Function TUSBTMCBase.FindUSBTMCInterfaces(Dev:Plibusb_device;Var Infos:TUSBTMCIntfInfos) : Boolean;
Var DeviceDesc : libusb_device_descriptor;
    NumConf    : Integer;
    Config     : Plibusb_config_descriptor;
    IIntf      : Integer;
    IAlt       : Integer;
Begin
  Result := False;
  { DeviceDescriptor.bDeviceClass = 0, .bDeviceSubClass = 0, .bDeviceProtocol = 0 }
  DeviceDesc := TLibUsbContext.GetDeviceDescriptor(Dev);
  if (DeviceDesc.bDeviceClass    <> 0) or
     (DeviceDesc.bDeviceSubClass <> 0) or
     (DeviceDesc.bDeviceProtocol <> 0) then Exit;
  // iterate over all configurations
  For NumConf := 0 to DeviceDesc.bNumConfigurations-1 do
    Begin
      Config := TLibUsbContext.GetConfigDescriptor(Dev,NumConf);
      // iterate over all interfaces
      For IIntf := 0 to Config^.bNumInterfaces-1 do
        With Config^._interface^[IIntf] do
          Begin
            // iterate over all alternate settings
            For IAlt := 0 to num_altsetting-1 do
              With altsetting^[IAlt] do
                Begin
                  { InterfaceDescriptor.bAlternateSetting = 0, .bInterfaceClass = $FE (Application specific), .bInterfaceSubClass = 3 (Test and Measurement) }
                  if (bAlternateSetting  <>   0) or
                     (bInterfaceClass    <> $FE) or   { Application specific }
                     (bInterfaceSubClass <>   3) then { Test and Measurement }
                    Continue;
                  { found one }
                  Result := True;
                  { store }
                  SetLength(Infos,Length(Infos)+1);
                  With Infos[Length(Infos)-1] do
                    Begin
                      Device         := Dev;
                      DevDescr       := DeviceDesc;
                      ConfigDescr    := Config^;
                      IntfDescr      := altsetting^[IAlt];
                    End;
                End;
          End;
    End;
End;

Type
  PMatchUSBTMCDeviceData = ^TMatchUSBTMCDeviceData;
  TMatchUSBTMCDeviceData = record
    Infos : ^TUSBTMCIntfInfos;
  End;

Function MatchUSBTMCDevice(Dev:Plibusb_device;Data:Pointer) : Boolean;
Var MyData : PMatchUSBTMCDeviceData;
Begin
  MyData := PMatchUSBTMCDeviceData(Data);
  Result := TUSBTMCBase.FindUSBTMCInterfaces(Dev,MyData^.Infos^);
End;

class Function TUSBTMCBase.Scan(AContext:TLibUsbContext) : TUSBTMCIntfInfos;
Var Infos : TUSBTMCIntfInfos;
    Data  : TMatchUSBTMCDeviceData;
Begin
  SetLength(Infos,0);
  Data.Infos:= @Infos;
  AContext.FindDevices(@MatchUSBTMCDevice,@Data);
  Result := Infos;
End;

class Function TUSBTMCBase.GetInfoStrings(AContext:TLibUsbContext;ConstRef IntfInfo:TUSBTMCIntfInfo) : TUSBTMCIntfInfoStrings;
Var Inst       : TLibUsbDevice;
Begin
  try
    Inst := TLibUsbDevice.Create(AContext,IntfInfo.Device);
    try
      With Result do
        Begin
          sVendor        := Inst.Control.GetString(IntfInfo.DevDescr.iManufacturer);
          sProduct       := Inst.Control.GetString(IntfInfo.DevDescr.iProduct);
          sSerial        := Inst.Control.GetString(IntfInfo.DevDescr.iSerialNumber);
          sConfiguration := Inst.Control.GetString(IntfInfo.ConfigDescr.iConfiguration);
          sInterface     := Inst.Control.GetString(IntfInfo.IntfDescr.iInterface);
        End;
    finally
      Inst.Free;
    End;
  except
    // nothing to do, just used this to c
  End;
End;

{ protected methods }

Function TUSBTMCBase.NextTag: Byte;
Begin
  FLastTag := FLastTag + 1;
  if FLastTag = 0 then
    FLastTag := 1;
  Result := FLastTag;
End;

{ getter and setter }

(**
 * Does the interface accept the INDICATOR_PULSE request?
 *
 * [USB-TMC] p. 29
 *)
Function TUSBTMCBase.GetCapIndicatorPulse: Boolean;
Begin
  Result := (FCapabilities.IntfCap and USBTMC_CAP_INTF_INDICATOR_PULS) <> 0;
End;

(**
 * Is the interface talk-only?
 *
 * [USB-TMC] p. 29
 *)
Function TUSBTMCBase.GetCapTalkOnly: Boolean;
Begin
  Result := (FCapabilities.IntfCap and USBTMC_CAP_INTF_TALK_ONLY) <> 0;
End;

(**
 * Is the interface listen-only?
 *
 * [USB-TMC] p. 29
 *)
Function TUSBTMCBase.GetCapListenOnly: Boolean;
Begin
  Result := (FCapabilities.IntfCap and USBTMC_CAP_INTF_LISTEN_ONLY) <> 0;
End;

(**
 * Does the device support ending a bulk IN transfer when a byte matches a
 * specified TermChar?
 *
 * [USB-TMC] p. 29
 *)
Function TUSBTMCBase.GetCapTermChar: Boolean;
Begin
  Result := (FCapabilities.DevCap and USBTMC_CAP_DEV_TERM_CHAR) <> 0;
End;

{ TUSBTMCUSB488 }

Constructor TUSBTMCUSB488.Create(AContext:TLibUsbContext;ADescriptor: TUSBTMCIntfInfo);
Begin
  inherited Create(AContext,ADescriptor);
  if FSubClass <> tcsUSB488 then
    raise Exception.CreateFmt('Sub class (%d) is not USB488.',[FSubClass]);
End;

Procedure TUSBTMCUSB488.Trigger;
Type
  TUSBTMCTrigger = packed record
    MsgID        : Byte;
    Tag          : Byte;
    TagInverse   : Byte;
    Reserved     : Array[0..7] of Byte;
  End;
Var Packet : TUSBTMCTrigger;
    Len    : LongInt;
    Result : LongInt;
Begin
  FillChar(Packet,SizeOf(Packet),0);
  Packet.MsgID        := UsbTmc488MsgID_Trigger;
  Packet.Tag          := NextTag;
  Packet.TagInverse   := not Packet.Tag;
  Result := FEPOut.Send(Packet,SizeOf(Packet),FTimeout);
  if Result <> Len then
    raise Exception.CreateFmt('Trigger: Len = %d should be %d, %s',[Result,Len,FContext.ErrorName(Result)]);
End;

Procedure TUSBTMCUSB488.Send(St:String);
Begin
  DevDepMsgOut(Length(St),UsbTmcTransferAttribute_EOM,St[1])
End;

Function TUSBTMCUSB488.Recv(MaxLen:Cardinal) : String;
Var Attr : Byte;
Begin
  SetLength(Result,MaxLen);
  RequestDevDepMsgIn(MaxLen,0,#0);
  DevDepMsgIn(MaxLen,Attr,Result[1]);
  SetLength(Result,MaxLen);
End;

{ getters and setters }

(**
 * Query BCD version number of the USB488 specification
 *
 * [USB488] p. 10
 *)
Function TUSBTMCUSB488.GetUsb488BcdVersion: Word;
Begin
  Result := TUSBTMCUSB488GetCapabilities(FCapabilities).BCDUSB488;
End;

(**
 * Is this a 488.2 USB488 interface?
 *
 * [USB488] p. 10
 *)
Function TUSBTMCUSB488.GetCapUSB4882: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488IntfCap and USB488_CAP_INTF_4882) <> 0;
End;

(**
 * Does inte interface accept REN_CONTROL, GO_TO_LOCAL and LOCAL_LOCKOUT request?
 *
 * [USB488] p. 10, 13ff
 *)
Function TUSBTMCUSB488.GetCapRenControl: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488IntfCap and USB488_CAP_INTF_REN_CONTROL) <> 0;
End;

(**
 * Does the interface support TRIGGER command messages?
 *
 * [USB488] p. 10
 *)
Function TUSBTMCUSB488.GetCapTrigger: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488IntfCap and USB488_CAP_INTF_TRIGGER) <> 0;
End;

(**
 * Does the device understand all mandatory SCPI commands?
 *
 * [USB488] p. 11
 *)
Function TUSBTMCUSB488.GetCapScpi: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488DevCap and USB488_CAP_DEV_SCPI) <> 0;
End;

(**
 * Is the device SR1 capable?
 *
 * If yes, the interface must have an Interrupt IN endpoint.
 *
 * [USB488] p. 11
 *)
Function TUSBTMCUSB488.GetCapSR1: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488DevCap and USB488_CAP_DEV_SR1) <> 0;
End;

(**
 * Is the device RL1 capable?
 *
 * If not, it is RL0 capable.
 *
 * [USB488] p. 11
 *)
Function TUSBTMCUSB488.GetCapRL1: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488DevCap and USB488_CAP_DEV_RL1) <> 0;
End;

(**
 * Is the device DT1 capable?
 *
 * If not, it is DT0 capable.
 *
 * [USB488] p. 11
 *)
Function TUSBTMCUSB488.GetCapDT1: Boolean;
Begin
  Result := (TUSBTMCUSB488GetCapabilities(FCapabilities).Usb488DevCap and USB488_CAP_DEV_DT1) <> 0;
End;

End.

