(**
 * Download a screenshot from Agilent InfiniiVision MSO-X 3000A oscilloscopes
 *
 * This test program can either communicate via USB or ethernet. Define either
 * USBTMC or TCP.
 * The TCP connection was not yet tested.
 *
 * USB: The USB bus is scanned for a device with the given idVendor and
 *   idProduct (see constants below). The first one found is used.
 *
 * TCP: The device with the given hostname or IP address is used.
 *
 * This program just downloads the currently shown screen and saves it to a
 * file. No settings on the scope are changes.
 *
 * Warning: This demo does no sanity checking. Be sure that you have setup a
 * serial decide bus.
 *
 *)
Program AgilentMSOX3000ASerialDownload;

{$mode objfpc}{$H+}
{$MODESWITCH NestedProcVars}

// either communicate via USB-TMC or TCP, define one of these two
{$DEFINE USBTMC}
{ $ DEFINE TCP}


Uses
  Classes, SysUtils, TypInfo, PasGpibUtils,
{$IFDEF USBTMC}
  LibUsbOop, UsbTmc, DevComUSBTMC,
{$ENDIF USBTMC}
{$IFDEF TCP}
  DevComTCP,
{$ENDIF TCP}
  AgilentMSOX3000A;

Const
{$IFDEF USBTMC}
  idVendor  : Integer = $0957;
  idProduct : Integer = $17A6;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Host      : String  = '192.168.87.166';
  Port      : Integer = 5025;
{$ENDIF TCP}

Var
{$IFDEF USBTMC}
  Context  : TLibUsbContext;
  Intf     : TUSBTMCIntfInfos;
  Tmc      : TUSBTMCUSB488;
  Comm     : TUSBTMCCommunicator;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Comm     : TTCPCommunicator;
{$ENDIF TCP}
  MSOX     : TAgilentMSOX3000A;
  I        : Integer;
  Source   : TWaveformSource;
  Filename : String;
  Waveform : TWaveform;

{$IFDEF USBTMC}
Procedure USBTMCErrorHandler;
Var I    : Integer;
    Code : Integer;
    Msg  : String;
Begin
  if not assigned(MSOX) then Exit;    // prevent accessing the device before its constructor has finished
  // print all errors in the queue
  For I := 0 to 20 do       // 34410A and 34461A can store up to 20 errors
    Begin
      {$IFDEF USBTMC}
      if (Tmc.ReadStatusByte and IEEE488_StatusByte_ErrorQueue) = 0 then Break;
      {$ENDIF}
      if I = 0 then
        WriteLn('Error Queue after last command '''+Comm.LastSend+''':');
      MSOX.GetNextError(Code,Msg);
      WriteLn('  ',Code,': ',Msg);
      if Code = 0 then Break;
    End;
End;
{$ENDIF USBTMC}

Procedure Usage(ExitCode:Integer);
Begin
  Write('Usage: ',ParamStr(0),' [-h|--help] ');
{$IFDEF USBTMC}
  Write('[-v idVendor] [-p idProduct] ');
{$ENDIF USBTMC}
{$IFDEF TCP}
  Write('[-t host:port] ');
{$ENDIF TCP}
  WriteLn('[-s sbus1|sbus2] [-o outfile]');
  WriteLn;
  WriteLn('  -h|--help  Prints this help');
{$IFDEF USBTMC}
  WriteLn('  -v         USB idVendor  in decimal or as 0xhhhh or $hhhh in hex,');
  WriteLn('             defaults to $',IntToHex(idVendor,4));
  WriteLn('  -p         USB idProduct in decimal or as 0xhhhh or $hhhh in hex,');
  WriteLn('             defaults to $',IntToHex(idProduct,4));
{$ENDIF USBTMC}
{$IFDEF TCP}
  WriteLn('  -t         TCP/IP host and port, defaults to ',Host,':',IntToStr(Port));
{$ENDIF TCP}
  WriteLn('  -o         Output file, defaults to ',Filename);
  if ExitCode >= 0 then
    Halt(ExitCode);
End;

Begin
  // prepare data and parse parameters
  Source := wsSBus1;
  Filename := 'screenshot-'+FormatDateTime('yyyymmdd-hhnnss',Now)+'.png';
  I := 1;
  while I <= ParamCount do
    Begin
      Case ParamStr(I) of
        '-h',
        '--help' : Usage(0);
{$IFDEF USBTMC}
        '-v'     : Begin Inc(I); idVendor  := StrToInt(ParamStr(I)); End;
        '-p'     : Begin Inc(I); idProduct := StrToInt(ParamStr(I)); End;
{$ENDIF USBTMC}
{$IFDEF TCP}
        '-t'     : Begin Inc(I); { TODO: set Host and Port } WriteLn('-t not yet implemented'); End;
{$ENDIF TCP}
        '-o'     : Begin Inc(I); Filename := ParamStr(I); End;
      else
        Usage(1);
      End;
      Inc(I);
    End;

  { device communicator }
{$IFDEF USBTMC}
  // device connector via USB-TMC
  Context := TLibUsbContext.Create;
  Intf := TUSBTMCUSB488.Scan(Context);
  if Length(Intf) = 0 then
    Begin
      WriteLn('Error: No USB devices with TMC and GPIB found');
      Halt;
    End;
  // search appropriate device in list and create UsbTmc handler
  For I := 0 to Length(Intf)-1 do
    if (Intf[I].DevDescr.idVendor  = idVendor) and
       (Intf[I].DevDescr.idProduct = idProduct) then
      Begin
        Tmc := TUSBTMCUSB488.Create(Context,Intf[I]);
        break;
      End;
  if not assigned(Tmc) then
    Begin
      WriteLn('Error: No matching USB devices ',IntToHex(idVendor,4),':',IntToHex(idProduct,4),' found');
      Halt;
    End;
  Comm := TUSBTMCCommunicator.Create(Tmc);
  Comm.SetTimeout(2000000{us});
  Comm.ErrorHandler := @USBTMCErrorHandler;
{$ENDIF USBTMC}
{$IFDEF TCP}
  // device connector via TCP/IP
  Comm := TTCPCommunicator.Create(Host,Port);
{$ENDIF TCP}
  Comm.SetTimeout(5000000{us});
  { remote instrument }
  MSOX := TAgilentMSOX3000A.Create(Comm);

  WriteLn('Download screenshot from ',MSOX.Identify,' to ',Filename);

  WriteLn('Saving screenshot to ',Filename);

  MSOX.SetHardcopyOptions(false);  // inksaver off: image is not inverted, i.e., mostly black
  //WriteLn('  Hardcopy Options: ',MSOX.GetHardcopyOptions);
  WriteData(Filename,MSOX.Screen(ifPng, ipColor));

  MSOX.Free;

  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USBTMC}
  Context.Free;
{$ENDIF USBTMC}
End.

