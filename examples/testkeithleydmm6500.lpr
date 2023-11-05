(**
 * Test program for Keithley DMM6500 6 1/2 Digit Multimeter
 *
 * This test program can either communicate via USB or ethernet. Define either
 * USBTMC or TCP. Please set the appropriate VISA identifier below.
 *
 * Port scan:
 *  23/tcp   open  telnet
 *  80/tcp   open  http
 *  111/tcp  open  rpcbind
 *  1024/tcp open  kdm
 *  1025/tcp open  NFS-or-IIS
 *  5025/tcp open  scpi-raw
 *  5030/tcp open  surfpass
 *  5044/tcp open  lxi-evntsvc
 *
 *)
Program TestKeithleyDMM6500;

{$mode objfpc}{$H+}

// either communicate via USB or TCP, define one of these two
{ $ DEFINE USBTMC}
{$DEFINE TCP}

// select which tests are performed
{$DEFINE TEST_SERIES}
{$DEFINE TEST_NPLC}
{$DEFINE TEST_APERTURE}
{$DEFINE TEST_RANGE}
{$DEFINE TEST_TSP_LINK}

Uses
  Classes, SysUtils, Math, PasGpibUtils, DevCom,
  DevComVisa,
  Keithley2600,
{$IFDEF USBTMC}
  UsbTmc, DevComUSBTMC,
{$ENDIF USBTMC}
{$IFDEF TCP}
  DevComTCP,
{$ENDIF TCP}
{$IFDEF TEST_TSP_LINK}
  KeithleyTSP,
{$ENDIF TEST_TSP_LINK}
  KeithleyDMM6500;

Const
{$IFDEF USBTMC}
  Visa = 'USB0::0x05e6::0x6500::04547924::INSTR';
{$ENDIF USBTMC}
{$IFDEF TCP}
  Visa = 'TCPIP::10.0.0.212::5025::SOCKET';
{$ENDIF TCP}
  TSPNodeID = 3;   // TSP-Link Node ID of the remote instrument

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

Procedure PrintAllEvents(DMM6500:TKeithleyDMM6500;LastCmd:String);
Var First       : Boolean;
    EventNumber : Integer;
    Message     : String;
    Severity,
    NodeID      : Integer;
    Time        : Double;
    SeveritySt  : String;
Begin
  if not assigned(DMM6500) then Exit;    // prevent accessing the device before its constructor has finished
  // print all events in the event queue
  First := True;
  While DMM6500.GetEventCount > 0 do
    Begin
      if First then
        WriteLn('Event Queue after last command '''+LastCmd+''':');
      First := False;
      DMM6500.GetNextEvent(EventNumber, Message, Severity, NodeID, Time);
      Case Severity of
       1 : SeveritySt := 'Error';
       2 : SeveritySt := 'Warning';
       4 : SeveritySt := 'Information';
      else
        SeveritySt := 'Undefined '+IntToStr(Severity);
      End;
      WriteLn('  ',EventNumber,': ',Message,' (severity ',SeveritySt,', node ',NodeID,', at ',Time:1:3,'s)');
      if EventNumber = 0 then Break;
    End;
End;


Var
  Comm          : IDeviceCommunicator;
  CommObj       : TObject;
{$IFDEF USBTMC}
  TmcComm       : TUSBTMCCommunicator;
  Status        : Byte;
{$ENDIF USBTMC}
  DMM6500       : TKeithleyDMM6500;
  DMM6500_Slave : TKeithleyDMM6500;
  V             : Double;
  MeasArr       : TDynDoubleArray;
  I             : Integer;
  St            : String;
{$IFDEF TEST_TSP_LINK}
  Nodes         : TNodeInfoArray;
{$ENDIF TEST_TSP_LINK}

{$IFDEF USBTMC}
Procedure USBTMCErrorHandler;
Begin
  if not assigned(DMM6500) then Exit;    // prevent accessing the device before its constructor has finished
  // print all events in the queue
  PrintAllEvents(DMM6500, {$IFDEF USBTMC}TmcComm.LastSend{$ELSE}''{$ENDIF USBTMC});
End;
{$ENDIF USBTMC}

Begin
  WriteLn('Contacting Keithley DMM6500 ', Visa);

  Comm := DevComOpen(Visa, CommObj);
  Comm.SetTimeout(10000000);

  DMM6500 := TKeithleyDMM6500.Create(Comm);

  WriteLn('Testing Keithley DMM6500 Interface');

{$IFDEF USBTMC}
  TmcComm := CommObj as TUSBTMCCommunicator;
  TmcComm.ErrorHandler := @USBTMCErrorHandler;
  // check residues from previous commands
  Status := TmcComm.Device.ReadStatusByte;
  if (Status and IEEE488_StatusByte_MessageAvailable) <> 0 then
    WriteLn('Old Reply: ',TmcComm.Receive);
  // print all errors in the queue
  if (Status and IEEE488_StatusByte_ErrorQueue) <> 0 then
    Begin
      WriteLn('There were errors!');
      USBTMCErrorHandler;
    End;
{$ENDIF USBTMC}

  // print all events
  WriteLn('Event Count: ',DMM6500.GetEventCount);
  PrintAllEvents(DMM6500, {$IFDEF USBTMC}TmcComm.LastSend{$ELSE}''{$ENDIF USBTMC});

  WriteLn('Resetting to default settings.');
  DMM6500.Reset;

  // print input terminals setting
  WriteLn('Input terminal selection: ',CInputTerminalsSettingNice[DMM6500.GetTerminals]);

  WriteLn('Testing display functions on user screen. Watch the display!');
  DMM6500.ChangeScreen('SCREEN_USER_SWIPE');
  DMM6500.SetText(1, 'Hello World!');
  DMM6500.SetText(2, 'And a happy new year! \018 \019 \020 \185 \021');
  Sleep(2000);
  DMM6500.ClearDisplay;

  WriteLn('Testing a simple measurement.');
  DMM6500.ChangeScreen('SCREEN_HOME');
  DMM6500.SetMeasureFunction('FUNC_DC_VOLTAGE');
  DMM6500.SetNPLC(1.0);
  DMM6500.EnableAutoRange(True);
  DMM6500.EnableAutoZero(True);
  DMM6500.AutoZero;
  WriteLn('Setting resolution to 6.5 digits');
  DMM6500.SetDisplayDigits(dg6_5);
  WriteLn('Disabling measure filters');
  DMM6500.EnableFilter(False);
  DMM6500.SetRange(10.0);
  V := DMM6500.Measure;
  WriteLn('Value = ',FloatToStr(V));

{$IFDEF TEST_SERIES}
  WriteLn;
  WriteLn('Starting measurement series with 10 measurements with NPLC=1.0');
  DMM6500.SetMeasureCount(10);
  DMM6500.ClearBuffer;
  Sleep(300);
  V := DMM6500.Measure;
  WriteLn('Value = ',FloatToStr(V));
  WriteLn('  total ',DMM6500.GetNumReadings);
  MeasArr := DMM6500.PrintBuffer;
  For I := 0 to Length(MeasArr)-1 do
    WriteLn('  ',FloatToStr(MeasArr[I]));
  WriteLn('  Num:    ',Length(MeasArr));
  WriteLn('  Min:    ',FloatToStrSI(MinValue(MeasArr),FormatSettings));
  WriteLn('  Max:    ',FloatToStrSI(MaxValue(MeasArr),FormatSettings));
  WriteLn('  Mean:   ',FloatToStrSI(Mean    (MeasArr),FormatSettings));
  WriteLn('  StdDev: ',FloatToStrSI(StdDev  (MeasArr),FormatSettings));
  // TODO: the device can do the statistics itself, wee buffer.getstats() [RM] p. 14-15
{$ENDIF TEST_SERIES}
{$IFDEF TEST_NPLC}
  WriteLn;
  WriteLn('Testing NPLC (0.0005 to 12 for 60Hz)');
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  0.0005);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  0.006);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  0.02);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  0.06);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  0.2);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  1.0);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'',  2.0);
  TestSetting(@DMM6500.SetNPLC,@DMM6500.GetNPLC,'', 12.0);
{$ENDIF TEST_NPLC}
{$IFDEF TEST_APERTURE}
  WriteLn;
  WriteLn('Testing Aperture (10µs to 0.24s for most functions at 50Hz)');
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s', 10.0E-6);
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s',100.0E-6);
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s',  1.0E-3);
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s',  1.234E-3);
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s', 10.0E-3);
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s',100.0E-3);
  TestSetting(@DMM6500.SetAperture,@DMM6500.GetAperture,'s',240.0E-3);
{$ENDIF TEST_APERTURE}
{$IFDEF TEST_RANGE}
  WriteLn;
  WriteLn('Testing Range (100mV to 1000V for DCV, 10µA to 3A for DCI)');
  // setup measurement to DC Volts
  DMM6500.SetMeasureFunction('FUNC_DC_VOLTAGE');
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'V',   0.1);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'V',   1.0);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'V',  10.0);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'V', 100.0);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'V',1000.0);
  // setup measurement to DC Amps
  DMM6500.SetMeasureFunction('FUNC_DC_CURRENT');
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',0.00001);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',0.0001);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',0.001);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',0.01);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',0.1);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',1.0);
  TestSetting(@DMM6500.SetRange,@DMM6500.GetRange,'A',3.0);
  // 10A only available at the rear terminal
  // setup measurement to DC Volts
  DMM6500.SetMeasureFunction('FUNC_DC_VOLTAGE');
{$ENDIF TEST_RANGE}
{$IFDEF TEST_TSP_LINK}
  WriteLn;
  WriteLn('Testing TSP-Link');
  I := DMM6500.TSPLinkInitialize;
  WriteLn('TSP-Link initialize found ',I,' nodes.');
  St := DMM6500.GetTSPLinkState;
  WriteLn('TSP-Link State is ',St);
  if St = 'online' then
    Begin
      // TSP-Link status
      WriteLn('Local node is ',  DMM6500.GetTSPLinkLocalNode);
      WriteLn('Master node is ', DMM6500.GetTSPLinkMasterNode);
      Nodes := DMM6500.GetTSPLinkNodes;
      WriteLn('The following nodes are in the TSP-Link system:');
      For I := 0 to Length(Nodes)-1 do
        With Nodes[I] do
          WriteLn('  ',Node:2,' ',Model,' ',SerialNo,' ',Version);

      // device
      DMM6500_Slave := TKeithleyDMM6500.Create(DMM6500, TSPNodeID);

      WriteLn('Input terminal selection: ',CInputTerminalsSettingNice[DMM6500_Slave.GetTerminals]);

      WriteLn('Testing display functions on user screen. Watch the display!');
      DMM6500_Slave.ChangeScreen('SCREEN_USER_SWIPE');
      DMM6500_Slave.SetText(1, 'Hello Remote World!');
      DMM6500_Slave.SetText(2, 'Nice to meet you!');
      Sleep(2000);
      DMM6500_Slave.ClearDisplay;

      WriteLn('Testing a simple measurement.');
      DMM6500_Slave.ChangeScreen('SCREEN_HOME');
      DMM6500_Slave.SetMeasureFunction('FUNC_DC_VOLTAGE');
      DMM6500_Slave.SetNPLC(1.0);
      DMM6500_Slave.EnableAutoRange(True);
      DMM6500_Slave.EnableAutoZero(True);
      DMM6500_Slave.AutoZero;
      DMM6500_Slave.SetDisplayDigits(dg6_5);
      DMM6500_Slave.EnableFilter(False);
      DMM6500_Slave.SetRange(10.0);
      V := DMM6500_Slave.Measure;
      WriteLn('Value = ',FloatToStr(V));

{$IFDEF TEST_SERIES}
      WriteLn;
      WriteLn('Starting measurement series with 10 measurements with NPLC=1.0');
      DMM6500_Slave.SetMeasureCount(10);
      DMM6500_Slave.ClearBuffer;
      Sleep(300);
      V := DMM6500_Slave.Measure;
      WriteLn('Value = ',FloatToStr(V));
      WriteLn('  total ',DMM6500_Slave.GetNumReadings);
      MeasArr := DMM6500_Slave.PrintBuffer;
      WriteLn('  Num:    ',Length(MeasArr));
      WriteLn('  Min:    ',FloatToStrSI(MinValue(MeasArr),FormatSettings));
      WriteLn('  Max:    ',FloatToStrSI(MaxValue(MeasArr),FormatSettings));
      WriteLn('  Mean:   ',FloatToStrSI(Mean    (MeasArr),FormatSettings));
      WriteLn('  StdDev: ',FloatToStrSI(StdDev  (MeasArr),FormatSettings));
      // TODO: the device can do the statistics itself, wee buffer.getstats() [RM] p. 14-15
{$ENDIF TEST_SERIES}

      FreeAndNil(DMM6500_Slave);
    End
  else
    Begin
      WriteLn('Skipping test.');
    End;

{$ENDIF TEST_TSP_LINK}

  DMM6500.Free;
  CommObj.Free;
End.

