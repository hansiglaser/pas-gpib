(**
 * Test program for Agilent 34410A digital multimeter
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
 * The test includes the reset to default settings, deactivating the beeper
 * and performing a test measurement.
 *
 * TCP Port Scan:
 *     80  (HTTP)
 *    111  (rpcbind)
 *    513  (login)
 *   1024  (kdm)
 *   5024  (SCPI TCPIP Socket)
 *   5025  (SCPI Telnet)
 *   5810  (VxWorks login)
 *)
Program TestAgilent34410A;

{$mode objfpc}{$H+}

// either communicate via USB or TCP, define one of these two
{ $ DEFINE USBTMC}
{$DEFINE TCP}

{ $ DEFINE TEST_NPLC}
{ $ DEFINE TEST_APERTURE}
{ $ DEFINE TEST_RANGE}
{ $ DEFINE TEST_SAMPLE_TIMER}

Uses
  Classes, SysUtils, DevCom,
{$IFDEF USBTMC}
  LibUsbOop, UsbTmc, DevComUSBTMC,
{$ENDIF USBTMC}
{$IFDEF TCP}
  DevComTCP,
{$ENDIF TCP}
  Agilent34410A;

Const
{$IFDEF USBTMC}
  idVendor  = $0957;
  idProduct = $0607;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Host = '192.168.0.2';
  Port = 5025;
{$ENDIF TCP}

Type
  TSetter = Procedure(ADouble:Double) of object;
  TGetter = Function : Double of object;

Procedure TestSetting(ASetter:TSetter;AGetter:TGetter;AUnit:String;ADouble:Double);
Var GDouble : Double;
Begin
  ASetter(ADouble);
  Sleep(100);
  if not assigned(AGetter) then
    Exit;
  GDouble := AGetter();
  if ((abs(ADouble) < 1E-25) and (abs(GDouble-ADouble) > 0.01)) or
     ((abs(ADouble) > 1E-25) and ((abs(GDouble-ADouble) / ADouble) > 0.01)) then
    WriteLn('Error: Set ',ADouble,' ',AUnit,', but got ',GDouble,' ',AUnit,'.');
End;

Var
{$IFDEF USBTMC}
  Context : TLibUsbContext;
  Intf    : TUSBTMCIntfInfos;
  I       : Integer;
  Tmc     : TUSBTMCUSB488;
  Comm    : TUSBTMCCommunicator;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Comm    : TTCPCommunicator;
{$ENDIF TCP}
  A34410A : TAgilent34410A;
  I       : Integer;

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
{$ENDIF USBTMC}
{$IFDEF TCP}
  // device connector via TCP/IP
  Comm := TTCPCommunicator.Create(Host,Port);
{$ENDIF TCP}

  A34410A := TAgilent34410A.Create(Comm);

  // reset to default settings
  A34410A.Reset;
  // disable the beeper
  A34410A.SetBeeper(false);
  // print input terminals setting
  WriteLn('Input terminal selection: ',CInputTerminalsSettingNice[A34410A.GetInputTerminalsSetting]);
  // setup measurement to DC Volts
  A34410A.SetSenseFunction(qtVoltageDC);

{$IFDEF TEST_NPLC}
  WriteLn('Testing NPLC');
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',  0.006);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',  0.02);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',  0.06);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',  0.2);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',  1.0);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',  2.0);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'', 10.0);
  TestSetting(@A34410A.SetNPLC,@A34410A.GetNPLC,'',100.0);
{$ENDIF TEST_NPLC}
{$IFDEF TEST_APERTURE}
  WriteLn('Testing Aperture');
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',100.0E-6);
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',1.0E-3);
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',1.234E-3);
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',10.0E-3);
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',100.0E-3);
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',234.5678E-3);
  TestSetting(@A34410A.SetAperture,@A34410A.GetAperture,'s',1.0);
{$ENDIF TEST_APERTURE}
{$IFDEF TEST_RANGE}
  WriteLn('Testing Range');
  // setup measurement to DC Volts
  A34410A.SetSenseFunction(qtVoltageDC);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'V',   0.1);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'V',   1.0);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'V',  10.0);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'V', 100.0);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'V',1000.0);
  // setup measurement to DC Amps
  A34410A.SetSenseFunction(qtCurrentDC);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'A',0.0001);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'A',0.001);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'A',0.01);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'A',0.1);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'A',1.0);
  TestSetting(@A34410A.SetRange,@A34410A.GetRange,'A',3.0);
  // setup measurement to DC Volts
  A34410A.SetSenseFunction(qtVoltageDC);
{$ENDIF TEST_RANGE}
{$IFDEF TEST_SAMPLE_TIMER}
  WriteLn('Testing Sample Timer');
  For I := 1 to 25 do
    TestSetting(@A34410A.SetSampleTimer,@A34410A.GetSampleTimer,'s',I * 20E-6);
  A34410A.SetSampleTimer(1.0);  // set back to default of 1s
{$ENDIF TEST_SAMPLE_TIMER}

  A34410A.SetNPLC(1.0);
  A34410A.AutoZero;
  A34410A.SetRange(10.0);  // 10.0V
  // perform measurement
  WriteLn('Measurement = ',A34410A.GetValue:1:5,' V');

  A34410A.Free;

  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USBTMC}
  Tmc.Free;
{$ENDIF USBTMC}
End.

