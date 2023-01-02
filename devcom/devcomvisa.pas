Unit DevComVisa;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils,
  DevCom;

(**
 * Create an IDeviceCommunicator connected to VISA Resource
 *
 * This function accepts a VISA Resource String to specify the connection to
 * an instrument. It returns an object which implements the interface
 * IDeviceCommunicator, which is connected to the instrument.
 *
 * For the VISA Resource syntax, please refer to e.g.,
 *   http://zone.ni.com/reference/en-XX/help/370131S-01/ni-visa/visaresourcesyntaxandexamples/
 *   https://pyvisa.readthedocs.io/en/stable/names.html#visa-resource-syntax-and-examples
 *
 * TODO:
 *  - the USB function DevComOpenUSB() has memory leaks
 *)
Function DevComOpen(VisaResource : String;Out CommObj:TObject) : IDeviceCommunicator;

Implementation
Uses
  LibUsbOop, UsbTmc, DevComUSBTMC,
  DevComTCP;

Function DevComOpenTCP(VisaResource : String; ResArr : TDynStringArray;Out CommObj:TObject) : IDeviceCommunicator;
Var Host       : String;
    Port       : LongInt;
    Comm       : TTCPCommunicator;
Begin
  // TCPIP INSTR:  'TCPIP[board]::host address[::LAN device name][::INSTR]' or
  // TCPIP SOCKET: 'TCPIP[board]::host address::port::SOCKET'
  if Length(ResArr[0]) > 5 then
    raise Exception.Create('Non-default board specifier in '+ResArr[0]+' not yet supported');
  if (Length(ResArr) < 2) or (Length(ResArr) > 4) then
    raise Exception.Create('Misformed VISA Resource string '''+VisaResource+'''');
  Host := ResArr[1];
  if ResArr[Length(ResArr)-1] = 'INSTR' then   // 'TCPIP[board]::host address[::LAN device name][::INSTR]'
    raise Exception.Create('LXI not yet supported')
  else if (Length(ResArr) <> 4) or (ResArr[Length(ResArr)-1] <> 'SOCKET') then  // 'TCPIP[board]::host address::port::SOCKET'
    raise Exception.Create('Misformed VISA Resource string '''+VisaResource+'''');
  Port := StrToInt(ResArr[2]);
  Comm := TTCPCommunicator.Create(Host,Port);
  CommObj := Comm;
  Result  := Comm;
End;

Function DevComOpenUSB(VisaResource : String; ResArr : TDynStringArray;Out CommObj:TObject) : IDeviceCommunicator;
Var Board      : Integer;
    idVendor   : Word;
    idProduct  : Word;
    sSerial    : String;
    bInterface : Integer;
    Context    : TLibUsbContext;
    IntfInfo   : TUSBTMCIntfInfos;
    Tmc        : TUSBTMCUSB488;
    Comm       : TUSBTMCCommunicator;
    I          : Integer;
    Status     : Byte;
Begin
  // USB INSTR: 'USB[board]::manufacturer ID::model code::serial number[::USB interface number][::INSTR]'
  // USB RAW:   'USB[board]::manufacturer ID::model code::serial number[::USB interface number]::RAW'
  if Length(ResArr[0]) > 3 then
    Begin
      Board := StrToInt(Copy(ResArr[0],4,Length(ResArr[0])));
      if Board <> 0 then
        raise Exception.Create('Non-default board specifier in '+ResArr[0]+' not yet supported');
    End;
  if (Length(ResArr) < 4) or (Length(ResArr) > 6) then
    raise Exception.Create('Misformed VISA Resource string '''+VisaResource+'''');
  if ResArr[Length(ResArr)-1] = 'RAW' then
    raise Exception.Create('Raw USB not yet supported');
  idVendor   := StrToInt(ResArr[1]);
  idProduct  := StrToInt(ResArr[2]);
  sSerial    := ResArr[3];
  bInterface := -1;    // none specified
  if (Length(ResArr) > 4) and (ResArr[4] <> 'INSTR') then
    bInterface := StrToInt(ResArr[4]);
  if (Length(ResArr) > 5) and (ResArr[5] <> 'INSTR') then
    raise Exception.Create('Misformed VISA Resource string '''+VisaResource+'''');
  // device connector via USB-TMC
  Context := TLibUsbContext.Create;
  IntfInfo := TUSBTMCUSB488.Scan(Context);
  if Length(IntfInfo) = 0 then
    Begin
      WriteLn('Error: No USB devices with TMC and GPIB found');
      Halt;
    End;
  // search appropriate device in list and create UsbTmc handler
  Tmc := Nil;
  For I := 0 to Length(IntfInfo)-1 do
    if (IntfInfo[I].DevDescr.idVendor  = idVendor) and
       (IntfInfo[I].DevDescr.idProduct = idProduct) and
       (Context.GetSerialNumber(IntfInfo[I].Device) = sSerial) and
       ((bInterface = -1) or (IntfInfo[I].IntfDescr.bInterfaceNumber = bInterface)) then
      Begin
        Tmc := TUSBTMCUSB488.Create(Context,IntfInfo[I]);
        break;
      End;
  if not assigned(Tmc) then
    Begin
      Context.Free;
      raise Exception.Create('Error: No matching USB device for '+VisaResource+' found');
    End;

  Comm := TUSBTMCCommunicator.Create(Tmc);
  CommObj := Comm;
  Result  := Comm;
  Result.SetTimeout(2000000{us});
//      Comm.ErrorHandler := @USBTMCErrorHandler;

  // TODO: Context.Free;
  // TODO: free IntfInfo list
End;

Function DevComOpen(VisaResource : String; Out CommObj : TObject) : IDeviceCommunicator;
Var ResArr : TDynStringArray;
Begin
  // VisaResource is e.g., 'TCPIP0::1.2.3.4::999::SOCKET' or 'USB::0x1234::125::A22-5::INSTR'
  ResArr := SplitStr('::',VisaResource);  // ResArr always has >= 1 entries

  // identify interface
  if Pos('TCPIP', ResArr[0]) = 1 then
    Begin
      Result := DevComOpenTCP(VisaResource, ResArr, CommObj);
    End
  else if Pos('USB', ResArr[0]) = 1 then
    Begin
      Result := DevComOpenUSB(VisaResource, ResArr, CommObj);
    End
  else
    raise Exception.Create('Interface type '+ResArr[0]+' is not yet supported');
End;

End.
