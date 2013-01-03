(**
 * Test program for Keithley 2600 SourceMeter SMU Instruments
 *
 * This test program can either communicate via USB or ethernet. Define either
 * USBTMC or TCP.
 *
 * USB: The USB bus is scanned for devices with USB TMC capability. From these
 *   the first one with the given idVendor and idProduct (see constants below)
 *   is used.
 *   -> The 2602A doesn't have a USB device port (but a host port).
 *   -> The 2602B has a USB device port at its back side.
 *
 * TCP: The device with the given hostname or IP address is used.
 *
 * Port scan:
 *  23/tcp   open  telnet
 *  80/tcp   open  http
 *  111/tcp  open  rpcbind
 *  1024/tcp open  kdm
 *  1025/tcp open  NFS-or-IIS
 *  5025/tcp open  unknown
 *  5026/tcp open  unknown
 *  5027/tcp open  unknown
 *  5030/tcp open  surfpass
 *  5044/tcp open  unknown
 *
 * The test includes the reset to default settings, deactivating the beeper,
 * setting up the measurement and source and performing a measurement. Finally
 * a string is displayed on the instrument display.
 *)
Program TestKeithley2600;

{$mode objfpc}{$H+}

// either communicate via USB or TCP, define one of these two
{ $ DEFINE USBTMC}
{$DEFINE TCP}

{ $ DEFINE TEST_MEASURE_DELAY}
{ $ DEFINE TEST_VOLTAGE_RANGE}
{ $ DEFINE TEST_CURRENT_RANGE}
{ $ DEFINE TEST_NPLC}
{ $ DEFINE TEST_VOLTAGE_LIMIT}
{ $ DEFINE TEST_CURRENT_LIMIT}
{ $ DEFINE TEST_OUTPUT_VOLTAGE}
{ $ DEFINE TEST_OUTPUT_CURRENT}

Uses
  Classes, SysUtils, Math, PasGpibUtils, DevCom,
{$IFDEF USBTMC}
  LibUsbOop, UsbTmc, DevComUSBTMC,
{$ENDIF USBTMC}
{$IFDEF TCP}
  DevComTCP,
{$ENDIF TCP}
  Keithley2600;

Const
{$IFDEF USBTMC}
  idVendor  = $1234;
  idProduct = $5678;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Host = '128.131.80.220';//'192.168.0.2';
  Port = 5025;
{$ENDIF TCP}

Procedure PrintAllErrors(K2600:TKeithley2600);
Var
  Code      : Integer;
  Message   : String;
  Severity,
  Node      : Integer;
Begin
  While K2600.ErrorCount > 0 do
    Begin
      K2600.GetNextError(Code,Message,Severity,Node);
      WriteLn('Error: code = ',Code,
              ', message = ''',Message,'''',
              ', severity = ',Severity,
              ', node = ',Node);
    End;
End;

Procedure PrintStatusRegisterSet(Comm:IDeviceCommunicator;Prefix:String;HexDigits:Integer);
Begin
  HexDigits := 4;
  WriteLn(Prefix,StringOfChar(' ',60-Length(Prefix)),' = ',
    '$',IntToHex(StrToInt(Comm.Query('print(tostring('+Prefix+'.condition))')),HexDigits),' ',
    '$',IntToHex(StrToInt(Comm.Query('print(tostring('+Prefix+'.enable))')),HexDigits),' ',
    '$',IntToHex(StrToInt(Comm.Query('print(tostring('+Prefix+'.event))')),HexDigits),' ',
    '$',IntToHex(StrToInt(Comm.Query('print(tostring('+Prefix+'.ntr))')),HexDigits),' ',
    '$',IntToHex(StrToInt(Comm.Query('print(tostring('+Prefix+'.ptr))')),HexDigits));
End;

Procedure PrintAllStatusRegisters(Comm:IDeviceCommunicator);
Begin
  WriteLn('status.condition                                             = ',Comm.Query('print(tostring(status.condition))'));
  PrintStatusRegisterSet(Comm,'status.measurement',4);
  PrintStatusRegisterSet(Comm,'status.measurement.buffer_available',1);
  PrintStatusRegisterSet(Comm,'status.measurement.current_limit',1);
  PrintStatusRegisterSet(Comm,'status.measurement.instrument',1);
  PrintStatusRegisterSet(Comm,'status.measurement.instrument.smua',3);
  PrintStatusRegisterSet(Comm,'status.measurement.instrument.smub',3);
  PrintStatusRegisterSet(Comm,'status.measurement.reading_overflow',1);
  PrintStatusRegisterSet(Comm,'status.measurement.voltage_limit',1);
  WriteLn('status.node_enable                                           = ',Comm.Query('print(tostring(status.node_enable))'));
  WriteLn('status.node_event                                            = ',Comm.Query('print(tostring(status.node_event))'));
  PrintStatusRegisterSet(Comm,'status.operation',4);
  PrintStatusRegisterSet(Comm,'status.operation.calibrating',1);
  PrintStatusRegisterSet(Comm,'status.operation.instrument',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.digio',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.digio.trigger_overrun',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.lan',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.lan.trigger_overrun',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.smua',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.smub',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.smua.trigger_overrun',2);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.smub.trigger_overrun',2);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.trigger_blender',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.trigger_blender.trigger_overrun',2);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.trigger_timer',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.trigger_timer.trigger_overrun',2);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.tsplink',4);
  PrintStatusRegisterSet(Comm,'status.operation.instrument.tsplink.trigger_overrun',2);
  PrintStatusRegisterSet(Comm,'status.operation.measuring',1);
  PrintStatusRegisterSet(Comm,'status.operation.remote',4);
  PrintStatusRegisterSet(Comm,'status.operation.sweeping',1);
  PrintStatusRegisterSet(Comm,'status.operation.trigger_overrun',4);
  PrintStatusRegisterSet(Comm,'status.operation.user',4);
  PrintStatusRegisterSet(Comm,'status.questionable',4);
  PrintStatusRegisterSet(Comm,'status.questionable.calibration',1);
  PrintStatusRegisterSet(Comm,'status.questionable.instrument',1);
  PrintStatusRegisterSet(Comm,'status.questionable.instrument.smua',4);
  PrintStatusRegisterSet(Comm,'status.questionable.instrument.smub',4);
  PrintStatusRegisterSet(Comm,'status.questionable.over_temperature',1);
  PrintStatusRegisterSet(Comm,'status.questionable.unstable_output',1);
//  WriteLn('*SRE?                 = ',Comm.Query('*SRE?'));
  WriteLn('status.request_enable                                        = ',Comm.Query('print(tostring(status.request_enable))'));
  WriteLn('status.request_event                                         = ',Comm.Query('print(tostring(status.request_event))'));
//  WriteLn('*STB?                 = ',Comm.Query('*STB?'));
  PrintStatusRegisterSet(Comm,'status.standard',2);
  PrintStatusRegisterSet(Comm,'status.system',4);
  PrintStatusRegisterSet(Comm,'status.system2',4);
  PrintStatusRegisterSet(Comm,'status.system3',4);
  PrintStatusRegisterSet(Comm,'status.system4',4);
  PrintStatusRegisterSet(Comm,'status.system5',4);
End;

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
  I       : Integer;
{$IFDEF USBTMC}
  Context : TLibUsbContext;
  Intf    : TUSBTMCIntfInfos;
  Tmc     : TUSBTMCUSB488;
  Comm    : TUSBTMCCommunicator;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Comm    : TTCPCommunicator;
{$ENDIF TCP}
  K2600   : TKeithley2600;
  KCh     : TKeithley2600Channel;
  Volt    : TDynDoubleArray;
  Curr    : TDynDoubleArray;

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
  Comm.SetTimeout(1000000);
{$ENDIF TCP}

  K2600 := TKeithley2600.Create(Comm);
  KCh := K2600.Channel[0];

  WriteLn('Testing Keithley 2600 Interface');

  WriteLn('Resetting to default settings.');
  K2600.Reset;
  WriteLn('Disabling the beeper.');
  K2600.SetBeeper(false);

  // setup
  WriteLn('Setting resolution to 6.5 digits');
  KCh.SetDisplayDigits(dg6_5);
  WriteLn('Disabling measure filters');
  KCh.EnableMeasureFilter(false);
  // Test a few functions
{$IFDEF TEST_MEASURE_DELAY}
  WriteLn('Testing measure delay');
  For I := 0 to 10 do
    TestSetting(@KCh.SetMeasureDelay,@KCh.GetMeasureDelay,'s',I * 10E-3);
{$ENDIF TEST_MEASURE_DELAY}
  WriteLn('Disabling measure delays');
  KCh.SetMeasureDelay(0.0);
  // measure functions
  WriteLn('Setting measure range to 6V and 1A without autorange');
  KCh.SetMeasureVoltageAutoRange(false);
  KCh.SetMeasureCurrentAutoRange(false);
{$IFDEF TEST_VOLTAGE_RANGE}
  WriteLn('Testing measurement voltage ranges');
  KCh.SetOutputFunction(ofDCAmps);
  TestSetting(@KCh.SetMeasureVoltageRange,@KCh.GetMeasureVoltageRange,'V', 0.1);
  TestSetting(@KCh.SetMeasureVoltageRange,@KCh.GetMeasureVoltageRange,'V', 1.0);
  TestSetting(@KCh.SetMeasureVoltageRange,@KCh.GetMeasureVoltageRange,'V', 6.0);
  TestSetting(@KCh.SetMeasureVoltageRange,@KCh.GetMeasureVoltageRange,'V',40.0);
{$ENDIF TEST_VOLTAGE_RANGE}
{$IFDEF TEST_CURRENT_RANGE}
  WriteLn('Testing measurement current ranges');
  KCh.SetOutputFunction(ofDCVolts);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',100E-9);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',  1E-6);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A', 10E-6);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',100E-6);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',  1E-3);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A', 10E-3);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',100E-3);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',  1.0);
  TestSetting(@KCh.SetMeasureCurrentRange,@KCh.GetMeasureCurrentRange,'A',  3.0);
{$ENDIF TEST_CURRENT_RANGE}
{$IFDEF TEST_NPLC}
  WriteLn('Testing integration time');
  TestSetting(@KCh.SetNPLC,@KCh.GetNPLC,'NPLCs', 0.001);
  TestSetting(@KCh.SetNPLC,@KCh.GetNPLC,'NPLCs', 0.01);
  TestSetting(@KCh.SetNPLC,@KCh.GetNPLC,'NPLCs', 0.12345678);
  TestSetting(@KCh.SetNPLC,@KCh.GetNPLC,'NPLCs', 1.0);
  TestSetting(@KCh.SetNPLC,@KCh.GetNPLC,'NPLCs',10.0);
  TestSetting(@KCh.SetNPLC,@KCh.GetNPLC,'NPLCs',25.0);
{$ENDIF TEST_NPLC}
  KCh.SetMeasureVoltageRange(6.0);   // V
  KCh.SetMeasureCurrentRange(1.0);   // A
  WriteLn('Setting integration time NPLC = 10');
  KCh.SetNPLC(10);
  WriteLn('Performing auto-zero');
  KCh.SetAutoZero(azOnce);
  // source functions
  WriteLn('Setting local sense mode');
  KCh.SetSenseMode(smLocal);
  WriteLn('Setting output function to voltage source');
  KCh.SetOutputFunction(ofDCVolts);
{$IFDEF TEST_VOLTAGE_LIMIT}
  WriteLn('Testing output voltage limit');
  TestSetting(@KCh.SetOutputVoltageLimit,@KCh.GetOutputVoltageLimit,'V',0.01);
  TestSetting(@KCh.SetOutputVoltageLimit,@KCh.GetOutputVoltageLimit,'V',0.1);
  TestSetting(@KCh.SetOutputVoltageLimit,@KCh.GetOutputVoltageLimit,'V',1.0);
  TestSetting(@KCh.SetOutputVoltageLimit,@KCh.GetOutputVoltageLimit,'V',5.0);
{$ENDIF TEST_VOLTAGE_LIMIT}
{$IFDEF TEST_CURRENT_LIMIT}
  WriteLn('Testing output current limit');
  TestSetting(@KCh.SetOutputCurrentLimit,@KCh.GetOutputCurrentLimit,'A',100E-9);
  TestSetting(@KCh.SetOutputCurrentLimit,@KCh.GetOutputCurrentLimit,'A',  1E-6);
  TestSetting(@KCh.SetOutputCurrentLimit,@KCh.GetOutputCurrentLimit,'A',0.1);
  TestSetting(@KCh.SetOutputCurrentLimit,@KCh.GetOutputCurrentLimit,'A',1.0);
  TestSetting(@KCh.SetOutputCurrentLimit,@KCh.GetOutputCurrentLimit,'A',3.0);
{$ENDIF TEST_CURRENT_LIMIT}
  WriteLn('Setting output limits to 5.5V and 0.9A');
  KCh.SetOutputVoltageLimit(5.5);
  KCh.SetOutputCurrentLimit(0.9);
  WriteLn('Disabling high-capacitance mode');
  KCh.SetHighCapacitanceMode(false);
{$IFDEF TEST_OUTPUT_VOLTAGE}
  WriteLn('Testing output voltage');
  TestSetting(@KCh.SetOutputVoltage,@KCh.GetOutputVoltage,'V',0.01);
  TestSetting(@KCh.SetOutputVoltage,@KCh.GetOutputVoltage,'V',0.1);
  TestSetting(@KCh.SetOutputVoltage,@KCh.GetOutputVoltage,'V',1.0);
  TestSetting(@KCh.SetOutputVoltage,@KCh.GetOutputVoltage,'V',5.0);
{$ENDIF TEST_OUTPUT_VOLTAGE}
{$IFDEF TEST_OUTPUT_CURRENT}
  WriteLn('Testing output current');
  TestSetting(@KCh.SetOutputCurrent,@KCh.GetOutputCurrent,'A',100E-9);
  TestSetting(@KCh.SetOutputCurrent,@KCh.GetOutputCurrent,'A',  1E-6);
  TestSetting(@KCh.SetOutputCurrent,@KCh.GetOutputCurrent,'A',0.1);
  TestSetting(@KCh.SetOutputCurrent,@KCh.GetOutputCurrent,'A',1.0);
  TestSetting(@KCh.SetOutputCurrent,@KCh.GetOutputCurrent,'A',3.0);
{$ENDIF TEST_OUTPUT_CURRENT}
  WriteLn('Setting output voltage to 1.0V');
  KCh.SetOutputVoltage(1.0);
  WriteLn('Enabling the output');
  KCh.EnableOutput(true);
  WriteLn('Checking output for overcurrent');
  if KCh.GetCompliance then   // true means that limiting values are active
    WriteLn('ERROR: Channel has overcurrent!');

  // print all errors
  WriteLn('Error Count: ',K2600.ErrorCount);
  PrintAllErrors(K2600);

  // measurement functions
  WriteLn('Starting 10 measurements');
  KCh.Measure(10,1,2);
  Write('Waiting until measurements are finished... ');
  While KCh.Measuring do
    Sleep(100);
  WriteLn('done.');
  WriteLn('Querying measurement values');
  Curr := KCh.GetBuffer(10,1);
  Volt := KCh.GetBuffer(10,2);
  WriteLn('Clearing buffers');
  KCh.ClearBuffer(1);
  KCh.ClearBuffer(2);
  WriteLn('==> V = ',Mean(Volt):1:3,'V, I = ',Mean(Curr):1:3,'A');
  // source functions
  KCh.EnableOutput(false);

  // display functions
  WriteLn('Clearing display');
  K2600.SelectDisplay(dsUser);
  K2600.ClearDisplay;
  WriteLn('Printing some text at the display');
  K2600.Print(1,1,'Hello World!');
  { Each display command seems to require a lot of time before it really
    updates the display, therefore the above stuff only works with looong
    Sleep() or when single-stepping while debugging. }

  K2600.Free;

  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USBTMC}
  Tmc.Free;
{$ENDIF USBTMC}
End.

(*

Examination of status registers while performing the measurement:

[hansi@hansi]-(/tmp)$ diff -U 0 statusregs-0.txt statusregs-1.txt
--- statusregs-0.txt	2013-01-02 17:01:32.000000000 +0100
+++ statusregs-1.txt	2013-01-02 16:56:01.000000000 +0100
@@ -3 +3 @@
-status.measurement.buffer_available                          = $0002 $0000 $0000 $0000 $0006
+status.measurement.buffer_available                          = $0000 $0000 $0000 $0000 $0006  -> condition: SMUA buffer not available anymore
@@ -6 +6 @@
-status.measurement.instrument.smua                           = $0100 $0000 $0000 $0000 $0183
+status.measurement.instrument.smua                           = $0000 $0000 $0000 $0000 $0183  -> condition: SMUA measurement event register is reset (buffer available bit)
@@ -19 +19 @@
-status.operation.instrument.smua                             = $0000 $0000 $0000 $0000 $0419
+status.operation.instrument.smua                             = $0010 $0000 $0010 $0000 $0419  -> condition & event: SMUA is taking an overlapped measurement
@@ -29 +29 @@
-status.operation.measuring                                   = $0000 $0000 $0000 $0000 $0006
+status.operation.measuring                                   = $0002 $0000 $0002 $0000 $0006  -> condition & event: identical to status.operation.instrument.smua bit B4
@@ -49 +48,0 @@
-Querying measurement values
[hansi@hansi]-(/tmp)$ diff -U 0 statusregs-1.txt statusregs-2.txt
--- statusregs-1.txt	2013-01-02 16:56:01.000000000 +0100
+++ statusregs-2.txt	2013-01-02 16:56:06.000000000 +0100
@@ -3 +3 @@
-status.measurement.buffer_available                          = $0000 $0000 $0000 $0000 $0006
+status.measurement.buffer_available                          = $0002 $0000 $0002 $0000 $0006  -> condition & event: buffer is available
@@ -6 +6 @@
-status.measurement.instrument.smua                           = $0000 $0000 $0000 $0000 $0183
+status.measurement.instrument.smua                           = $0100 $0000 $0100 $0000 $0183  -> condition & event: buffer is available
@@ -19 +19 @@
-status.operation.instrument.smua                             = $0010 $0000 $0010 $0000 $0419
+status.operation.instrument.smua                             = $0010 $0000 $0000 $0000 $0419  -> event: taking overlapped measurements vanished
@@ -29 +29 @@
-status.operation.measuring                                   = $0002 $0000 $0002 $0000 $0006
+status.operation.measuring                                   = $0002 $0000 $0000 $0000 $0006  -> event: identical to status.operation.instrument.smua bit B4
[hansi@hansi]-(/tmp)$ diff -U 0 statusregs-2.txt statusregs-3.txt
--- statusregs-2.txt	2013-01-02 16:56:06.000000000 +0100
+++ statusregs-3.txt	2013-01-02 16:56:13.000000000 +0100
@@ -3 +3 @@
-status.measurement.buffer_available                          = $0002 $0000 $0002 $0000 $0006
+status.measurement.buffer_available                          = $0002 $0000 $0000 $0000 $0006  -> event: SMUA buffer not available anymore
@@ -6 +6 @@
-status.measurement.instrument.smua                           = $0100 $0000 $0100 $0000 $0183
+status.measurement.instrument.smua                           = $0100 $0000 $0000 $0000 $0183  -> event: buffer is available event vanished
[hansi@hansi]-(/tmp)$ diff -U 0 statusregs-3.txt statusregs-4.txt
--- statusregs-3.txt	2013-01-02 16:56:13.000000000 +0100
+++ statusregs-4.txt	2013-01-02 16:56:21.000000000 +0100
@@ -19 +19 @@
-status.operation.instrument.smua                             = $0010 $0000 $0000 $0000 $0419
+status.operation.instrument.smua                             = $0000 $0000 $0000 $0000 $0419  -> condition: taking overlapped measurements vanished
@@ -29 +29 @@
-status.operation.measuring                                   = $0002 $0000 $0000 $0000 $0006
+status.operation.measuring                                   = $0000 $0000 $0000 $0000 $0006  -> condition: taking overlapped measurements vanished

*)
