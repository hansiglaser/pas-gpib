(**
 * Test program for Keysight E3631xA Programmable DC Power Supplies
 *
 * This test program can either communicate via USB or ethernet. Define either
 * USBTMC or TCP.
 *
 * USB: The USB bus is scanned for devices with USB TMC capability. From these
 *   the first one with the given idVendor and idProduct (see constants below)
 *   is used.
 *
 * TCP: The device with the given hostname or IP address is used.
 *
 *)
Program TestKeysightE3631xA;

{$mode objfpc}{$H+}

// select device type, define one of these:
{$DEFINE KeysightE3631x}

// either communicate via USB or TCP, define one of these two:
{$DEFINE USBTMC}
{ $ DEFINE TCP}

{$DEFINE TEST_SOURCE}
{$DEFINE TEST_MEASURE}
{$DEFINE TEST_SENSE}
{$DEFINE TEST_PAIR}

Uses
  Classes, SysUtils, DevCom,
{$IFDEF USBTMC}
  LibUsbOop, UsbTmc, DevComUSBTMC,
{$ENDIF USBTMC}
{$IFDEF TCP}
  DevComTCP,
{$ENDIF TCP}
  KeysightE3631xA;

Const
{$IFDEF USBTMC}
  {$IFDEF KeysightE3631x}
  idVendor  = $2A8D;
  idProduct = $1202;
  {$ENDIF}
{$ENDIF USBTMC}
{$IFDEF TCP}
  Host = '192.168.0.2';
  Port = 5025;
{$ENDIF TCP}

Var
{$IFDEF USBTMC}
  Context : TLibUsbContext;
  Intf    : TUSBTMCIntfInfos;
  Tmc     : TUSBTMCUSB488;
  Comm    : TUSBTMCCommunicator;
  I       : Integer;
  Status  : Byte;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Comm    : TTCPCommunicator;
{$ENDIF TCP}
  E3631xA : TKeysightE3631xA;

{$IFDEF USBTMC}
Procedure USBTMCErrorHandler;
Var I    : Integer;
    Code : Integer;
    Msg  : String;
Begin
  if not assigned(E3631xA) then Exit;    // prevent accessing the device before its constructor has finished
  // print all errors in the queue
  For I := 0 to 20 do       // 34410A and 34461A can store up to 20 errors
    Begin
      {$IFDEF USBTMC}
      if (Tmc.ReadStatusByte and IEEE488_StatusByte_ErrorQueue) = 0 then Break;
      {$ENDIF}
      if I = 0 then
        WriteLn('Error Queue:');
      E3631xA.GetNextError(Code,Msg);
      WriteLn('  ',Code,': ',Msg);
      if Code = 0 then Break;
    End;
End;
{$ENDIF USBTMC}

Begin
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

  E3631xA := TKeysightE3631xA.Create(Comm);
  WriteLn('Connected to device ',E3631xA.Identify);

{$IFDEF USBTMC}
  // check residues from previous commands
  Status := Tmc.ReadStatusByte;
  if (Status and IEEE488_StatusByte_MessageAvailable) <> 0 then
    WriteLn('Old Reply: ',Comm.Receive);
  // print all errors in the queue
  if (Status and IEEE488_StatusByte_ErrorQueue) <> 0 then
    USBTMCErrorHandler;
{$ENDIF USBTMC}

  WriteLn('Reset to default settings');
  E3631xA.Reset;
  WriteLn('Disable the beeper');
  E3631xA.SetBeeper(false);

{$IFDEF TEST_SOURCE}
  WriteLn('Set output voltage and enable the output');
  E3631xA.SetVoltage([1,2,3],5.12345);
  E3631xA.EnableOutput([1,2,3],True);
  Sleep(500);
  WriteLn('Set current limit');
  E3631xA.SetCurrentLimit([1,2,3],0.4321);
  Sleep(500);
{$ENDIF TEST_SOURCE}

{$IFDEF TEST_MEASURE}
  WriteLn('Measure all voltages and currents');
  // note: measureing is very slow, especially when using the multi-channel function, where even a timeout of 1s is too small
  WriteLn('  Ch1: V = ',E3631xA.MeasureVoltage(1):1:5,'V, I = ',E3631xA.MeasureCurrent(1):1:5,'A');
  WriteLn('  Ch2: V = ',E3631xA.MeasureVoltage(2):1:5,'V, I = ',E3631xA.MeasureCurrent(2):1:5,'A');
  WriteLn('  Ch3: V = ',E3631xA.MeasureVoltage(3):1:5,'V, I = ',E3631xA.MeasureCurrent(3):1:5,'A');
//  Voltages := E3631xA.MeasureVoltage([1,2,3]);
//  Currents := E3631xA.MeasureCurrent([1,2,3]);
{$ENDIF TEST_MEASURE}

{$IFDEF TEST_SENSE}
  WriteLn('Test remote and local sensing');
  E3631xA.SetSenseMode([1],smRemote);
  Sleep(1000);
  E3631xA.SetSenseMode([1],smLocal);
  Sleep(500);
{$ENDIF TEST_SENSE}

{$IFDEF TEST_PAIR}
  WriteLn('Test pairing of Ch2&Ch3');
  E3631xA.SetPairMode(pmOff);
  Sleep(500);
  E3631xA.SetPairMode(pmParallel);
  Sleep(500);
  E3631xA.SetPairMode(pmSeries);
  Sleep(500);
  E3631xA.SetPairMode(pmOff);
  Sleep(500);

  WriteLn('Test tracking of Ch2&Ch3');
  E3631xA.SetTrackMode(True);
  Sleep(500);
  E3631xA.SetTrackMode(False);
  Sleep(500);
{$ENDIF TEST_PAIR}

  WriteLn('Done');
  E3631xA.Free;

  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USBTMC}
  Tmc.Free;
{$ENDIF USBTMC}
End.

