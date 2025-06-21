(**
 * Test program for Keithley 2450 SourceMeter SMU Instruments
 *
 * Please set the VISA identifier below.
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
 * The test includes the reset to default settings, deactivating the beeper,
 * setting up the measurement and source and performing a measurement. Finally
 * a string is displayed on the instrument display.
 *)
Program TestKeithley2450;

{$mode objfpc}{$H+}

{$DEFINE TEST_MEASURE_VOLTAGE_RANGE}
{$DEFINE TEST_MEASURE_CURRENT_RANGE}
{$DEFINE TEST_NPLC}
{$DEFINE TEST_SOURCE_DELAY}
{$DEFINE TEST_SOURCE_VOLTAGE_RANGE}
{$DEFINE TEST_SOURCE_CURRENT_RANGE}
{$DEFINE TEST_SOURCE_VOLTAGE_LIMIT}
{$DEFINE TEST_SOURCE_CURRENT_LIMIT}
{$DEFINE TEST_OUTPUT_VOLTAGE}
{$DEFINE TEST_OUTPUT_CURRENT}
{$DEFINE TEST_ENABLE_OUTPUT}
{$DEFINE TEST_SET_TEXT}

Uses
  Classes, SysUtils, Math, PasGpibUtils, DevCom,
  DevComVisa,
  LibUsbOop, UsbTmc, DevComUSBTMC,
  DevComTCP,
{$IFDEF TEST_TSP_LINK}
  KeithleyTSP,
{$ENDIF TEST_TSP_LINK}
  Keithley2450;

Const
//Visa = 'USB0::0x05e6::0x6500::*::INSTR';
  Visa = 'TCPIP::10.0.0.57::5025::SOCKET';

Procedure PrintAllEvents(K2450:TKeithley2450;LastCmd:String='');
Var EventNumber : Integer;
    Message     : String;
    Severity,
    NodeID      : Integer;
    Time        : Double;
    SeveritySt  : String;
Begin
  if not assigned(K2450) then Exit;    // prevent accessing the device before its constructor has finished
  if K2450.GetEventCount = 0 then Exit;
  // print all events in the event queue
  if LastCmd > '' then
    WriteLn('Event Queue after last command '''+LastCmd+''':')
  else
    WriteLn('Event Queue:');
  While K2450.GetEventCount > 0 do
    Begin
      K2450.GetNextEvent(EventNumber, Message, Severity, NodeID, Time);
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

Procedure PrintStatusRegister(Comm:IDeviceCommunicator;RegName:String;HexDigits:Integer);
Var St : String;
    I  : LongInt;
Begin
  St := Comm.Query('print(tostring('+RegName+'))');
  if TryStrToInt(St, I) then
    St := '$'+IntToHex(I, HexDigits);
  WriteLn(RegName,StringOfChar(' ',40-Length(RegName)),' = ',St);
End;

Procedure PrintAllStatusRegisters(Comm:IDeviceCommunicator);
Begin
  PrintStatusRegister(Comm,'status.condition',2);
  PrintStatusRegister(Comm,'status.operation.condition',4);
  PrintStatusRegister(Comm,'status.operation.enable',4);
  PrintStatusRegister(Comm,'status.operation.event',4);
  PrintStatusRegister(Comm,'status.questionable.condition',4);
  PrintStatusRegister(Comm,'status.questionable.enable',4);
  PrintStatusRegister(Comm,'status.questionable.event',4);
//  WriteLn('*SRE?                 = ',Comm.Query('*SRE?'));
  PrintStatusRegister(Comm,'status.request_enable',2);
//  WriteLn('*STB?                 = ',Comm.Query('*STB?'));
  PrintStatusRegister(Comm,'status.standard.enable',2);
  PrintStatusRegister(Comm,'status.standard.event',2);
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
    WriteLn('  ERROR: Set ',FloatToStrSI(ADouble, FormatSettings),AUnit,', but got ',FloatToStrSI(GDouble, FormatSettings),AUnit,'.')
  else
    WriteLn('  Good:  Set ',FloatToStrSI(ADouble, FormatSettings),AUnit,', got ',FloatToStrSI(GDouble, FormatSettings),AUnit,'.')
End;

Var
  Comm    : IDeviceCommunicator;
  CommObj : TObject;
  TmcComm : TUSBTMCCommunicator;
  Status  : Byte;
  K2450   : TKeithley2450;
{$IFDEF TEST_SOURCE_DELAY}
  I       : Integer;
{$ENDIF TEST_SOURCE_DELAY}
{$IFDEF TEST_ENABLE_OUTPUT}
  Volt    : TDynDoubleArray;
  Curr    : TDynDoubleArray;
{$ENDIF TEST_ENABLE_OUTPUT}

Procedure USBTMCErrorHandler;
Begin
  if not assigned(K2450) then Exit;    // prevent accessing the device before its constructor has finished
  // print all events in the queue
  PrintAllEvents(K2450, TmcComm.LastSend);
End;

Begin
  WriteLn('Contacting Keithley 2450 ', Visa);

  Comm := DevComOpen(Visa, CommObj);
  Comm.SetTimeout(10000000);

  K2450 := TKeithley2450.Create(Comm);

  WriteLn('Testing Keithley 2450 Interface');

  if CommObj is TUSBTMCCommunicator then
    Begin
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
    End;

  // print all events
  WriteLn('Event Count: ',K2450.GetEventCount);
  PrintAllEvents(K2450);

  WriteLn('Resetting to default settings.');
  K2450.Reset;

  // setup
  WriteLn('Setting resolution to 6.5 digits');
  K2450.SetMeasureDisplayDigits(dg6_5);
  WriteLn('Disabling measure filters');
  K2450.EnableMeasureFilter(false);
  PrintAllEvents(K2450,'EnableMeasureFilter');
  // measure functions
  K2450.EnableMeasureAutoRange(false);
  PrintAllEvents(K2450,'EnableMeasureAutoRange');
{$IFDEF TEST_MEASURE_VOLTAGE_RANGE}
  WriteLn('Testing measurement voltage ranges');
  K2450.SetMeasureFunction(mfDCVolts);
  K2450.SetSourceFunction(sfDCAmps);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'V', 20E-3);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'V',200E-3);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'V',  2.0);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'V', 20.0);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'V',200.0);
  PrintAllEvents(K2450,'GetMeasureRange with mfDCVolts');
{$ENDIF TEST_MEASURE_VOLTAGE_RANGE}
{$IFDEF TEST_MEASURE_CURRENT_RANGE}
  WriteLn('Testing measurement current ranges');
  K2450.SetMeasureFunction(mfDCAmps);
  K2450.SetSourceFunction(sfDCVolts);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A', 10E-9);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A',100E-9);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A',  1E-6);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A', 10E-6);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A',100E-6);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A',  1E-3);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A', 10E-3);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A',100E-3);
  TestSetting(@K2450.SetMeasureRange,@K2450.GetMeasureRange,'A',  1.0);
  PrintAllEvents(K2450,'GetMeasureRange with mfDCAmps');
{$ENDIF TEST_MEASURE_CURRENT_RANGE}
{$IFDEF TEST_NPLC}
  WriteLn('Testing integration time');
  TestSetting(@K2450.SetNPLC,@K2450.GetNPLC,'NPLCs', 0.01);
  TestSetting(@K2450.SetNPLC,@K2450.GetNPLC,'NPLCs', 0.12345678);
  TestSetting(@K2450.SetNPLC,@K2450.GetNPLC,'NPLCs', 1.0);
  TestSetting(@K2450.SetNPLC,@K2450.GetNPLC,'NPLCs',10.0);
  PrintAllEvents(K2450,'GetNPLC');
{$ENDIF TEST_NPLC}
  WriteLn('Setting integration time NPLC = 10');
  K2450.SetNPLC(10);
  WriteLn('Performing auto-zero');
  K2450.MeasureAutoZeroOnce;
  PrintAllEvents(K2450,'MeasureAutoZeroOnce');
  // source functions
  WriteLn('Setting local sense mode');
  K2450.SetSenseMode(sm2Wire);
  WriteLn('Setting output function to voltage source');
  K2450.SetSourceFunction(sfDCVolts);
  PrintAllEvents(K2450,'SetSourceFunction');
{$IFDEF TEST_SOURCE_DELAY}
  WriteLn('Testing source delay');
  For I := 0 to 10 do
    TestSetting(@K2450.SetSourceDelay,@K2450.GetSourceDelay,'s',I * 10E-3);
  PrintAllEvents(K2450,'GetSourceDelay');
{$ENDIF TEST_SOURCE_DELAY}
  WriteLn('Disabling source delays');
  K2450.SetSourceDelay(0.0);
{$IFDEF TEST_SOURCE_VOLTAGE_RANGE}
  WriteLn('Testing source voltage ranges');
  K2450.SetMeasureFunction(mfDCAmps);
  K2450.SetSourceFunction(sfDCVolts);
  // set a measure range which does not exceed the power limit of 22W at 200V
  K2450.SetMeasureRange(100E-3);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'V', 20E-3);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'V',200E-3);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'V',  2.0);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'V', 20.0);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'V',200.0);
  PrintAllEvents(K2450,'GetSourceRange with mfDCVolts');
{$ENDIF TEST_SOURCE_VOLTAGE_RANGE}
{$IFDEF TEST_SOURCE_CURRENT_RANGE}
  WriteLn('Testing source current ranges');
  K2450.SetMeasureFunction(mfDCVolts);
  K2450.SetSourceFunction(sfDCAmps);
  // set a measure range which does not exceed the power limit of 22W at 1A
  K2450.SetMeasureRange(20.0);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A', 10E-9);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A',100E-9);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A',  1E-6);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A', 10E-6);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A',100E-6);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A',  1E-3);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A', 10E-3);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A',100E-3);
  TestSetting(@K2450.SetSourceRange,@K2450.GetSourceRange,'A',  1.0);
  PrintAllEvents(K2450,'GetSourceRange with mfDCAmps');
{$ENDIF TEST_SOURCE_CURRENT_RANGE}
{$IFDEF TEST_SOURCE_VOLTAGE_LIMIT}
  WriteLn('Testing output voltage limit');
  K2450.SetMeasureFunction(mfDCVolts);
  K2450.SetSourceFunction(sfDCAmps);
  K2450.SetMeasureRange(0.02);
  TestSetting(@K2450.SetSourceVoltageLimit,@K2450.GetSourceVoltageLimit,'V',0.01);
  K2450.SetMeasureRange(0.2);
  TestSetting(@K2450.SetSourceVoltageLimit,@K2450.GetSourceVoltageLimit,'V',0.1);
  K2450.SetMeasureRange(2.0);
  TestSetting(@K2450.SetSourceVoltageLimit,@K2450.GetSourceVoltageLimit,'V',1.0);
  K2450.SetMeasureRange(20.0);
  TestSetting(@K2450.SetSourceVoltageLimit,@K2450.GetSourceVoltageLimit,'V',5.0);
  PrintAllEvents(K2450,'GetSourceVoltageLimit');
{$ENDIF TEST_SOURCE_VOLTAGE_LIMIT}
{$IFDEF TEST_SOURCE_CURRENT_LIMIT}
  WriteLn('Testing output current limit');
  K2450.SetMeasureFunction(mfDCAmps);
  K2450.SetSourceFunction(sfDCVolts);
  // set a source range which does not exceed the power limit of 22W at 1A
  K2450.SetSourceRange(20.0);
  // set a measure range which does not exceed the power limit of 22W at 1A
  K2450.SetMeasureRange(1E-6);
  TestSetting(@K2450.SetSourceCurrentLimit,@K2450.GetSourceCurrentLimit,'A',100E-9);
  K2450.SetMeasureRange(1E-5);
  TestSetting(@K2450.SetSourceCurrentLimit,@K2450.GetSourceCurrentLimit,'A',  1E-6);
  K2450.SetMeasureRange(1.0);
  TestSetting(@K2450.SetSourceCurrentLimit,@K2450.GetSourceCurrentLimit,'A',0.1);
  TestSetting(@K2450.SetSourceCurrentLimit,@K2450.GetSourceCurrentLimit,'A',1.0);
  PrintAllEvents(K2450,'GetSourceCurrentLimit');
{$ENDIF TEST_SOURCE_CURRENT_LIMIT}
  WriteLn('Disabling high-capacitance mode');
  K2450.SetHighCapacitanceMode(false);
  PrintAllEvents(K2450,'SetHighCapacitanceMode');
{$IFDEF TEST_OUTPUT_VOLTAGE}
  WriteLn('Testing output voltage');
  K2450.SetSourceFunction(sfDCVolts);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'V',0.01);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'V',0.1);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'V',1.0);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'V',5.0);
  PrintAllEvents(K2450,'GetSouceLevel');
{$ENDIF TEST_OUTPUT_VOLTAGE}
{$IFDEF TEST_OUTPUT_CURRENT}
  WriteLn('Testing output current');
  K2450.SetSourceFunction(sfDCAmps);
  K2450.SetSourceRange(1E-6);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'A',100E-9);
  K2450.SetSourceRange(1E-5);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'A',  1E-6);
  K2450.SetSourceRange(1.0);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'A',0.1);
  TestSetting(@K2450.SetSourceLevel,@K2450.GetSouceLevel,'A',1.0);
  PrintAllEvents(K2450,'GetSouceLevel');
{$ENDIF TEST_OUTPUT_CURRENT}
{$IFDEF TEST_ENABLE_OUTPUT}
  WriteLn('Setting output voltage to 1.0V');
  K2450.EnableSourceAutoRange(true);
  K2450.EnableMeasureAutoRange(true);
  K2450.SetMeasureFunction(mfDCAmps);
  K2450.SetSourceFunction(sfDCVolts);
  K2450.SetSourceCurrentLimit(1E-4);
  K2450.SetSourceLevel(1.0);     // Volt
  WriteLn('Enabling the output');
  K2450.EnableOutput(true);
  // The output actually stays disabled and is only enabled if a measurement is performed.
  // Also "K2450.DevCom.Send('trigger.model.initiate()');" doesn't help and gives an error "2710: Unable to initiate, no block number 1 found in trigger model"
  K2450.Measure('defbuffer1');
  K2450.ClearBuffer('defbuffer1');
  WriteLn('Checking output for overcurrent');
  if K2450.GetSourceCurrentLimitTripped then   // true means that limiting values are active
    WriteLn('ERROR: Source has overcurrent!');
  PrintAllEvents(K2450,'GetSourceCurrentLimitTripped');
  WriteLn('Starting 10 measurements');
  K2450.SetMeasureCount(10);
  K2450.Measure('defbuffer1');
  WriteLn('The buffer contains ',K2450.DevCom.Query('print(defbuffer1.n)'),' entries');
  WriteLn('Querying measurement values');
  Curr := K2450.GetBuffer(1, 10, 'defbuffer1.readings');
  Volt := K2450.GetBuffer(1, 10, 'defbuffer1.sourcevalues');
  WriteLn('Clearing buffer');
  K2450.ClearBuffer('defbuffer1');
  WriteLn('==> V = ',FloatToStrSI(Mean(Volt), FormatSettings),' ± ',FloatToStrSI(StdDev(Volt), FormatSettings),' V,',
             ' I = ',FloatToStrSI(Mean(Curr), FormatSettings),' ± ',FloatToStrSI(StdDev(Curr), FormatSettings),' A');
  // source functions
  K2450.EnableOutput(false);
{$ENDIF TEST_ENABLE_OUTPUT}

{$IFDEF TEST_SET_TEXT}
  // display functions
  WriteLn('Clearing display');
  K2450.ChangeScreen('SCREEN_USER_SWIPE');
  K2450.ClearDisplay;
  WriteLn('Printing some text at the display');
  K2450.SetText(1,'Hello World!');
{$ENDIF TEST_SET_TEXT}
  WriteLn('Status registers:');
  PrintAllStatusRegisters(K2450.DevCom);

  K2450.Free;
  CommObj.Free;
End.

