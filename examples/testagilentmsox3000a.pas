(**
 * Demonstrate remote control of the Agilent InfiniiVision MSO-X 3000A oscilloscopes
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
 * At the beginning the scope is setup. Then several tests are performed,
 * which can be enabled or disabled with the TEST_* defines.
 * For a screenshot the user is asked to connect the
 * probe of channel 1 to the square wave calibration output. After pressing
 * [Enter], a screenshot is saved.
 *)
Program TestAgilentMSOX3000A;

{$mode objfpc}{$H+}
{$MODESWITCH NestedProcVars}

// either communicate via USB-TMC or TCP, define one of these two
{$DEFINE USBTMC}
{ $ DEFINE TCP}

// define 0-n of the following to enable/disable the respective tests
{ $ DEFINE TEST_TDIV}
{ $ DEFINE TEST_TRIGGER_LEVEL}
{ $ DEFINE TEST_VDIV}
{ $ DEFINE TEST_VOFST}
{ $ DEFINE TEST_MEASURE}
{ $ DEFINE TEST_LONG_RECORD}
{ $ DEFINE TEST_SCREENSHOT}
{$DEFINE TEST_WAVEFORM}

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
  idVendor  = $0957;
  idProduct = $17A6;
{$ENDIF USBTMC}
{$IFDEF TCP}
  Host = '192.168.87.166';
  Port = 5025;
{$ENDIF TCP}

Type
  TSetter = Procedure(ADouble:Double) of object;
  TGetter = Function : Double of object;

Procedure TestSetting(ASetter:TSetter;AGetter:TGetter;AUnit:String;ADouble:Double);
Var GDouble : Double;
Begin
  WriteLn('Testing ',ADouble,' ',AUnit);
  ASetter(ADouble);
  Sleep(200);   // with shorter delay the scope times out after ~20-50 commands and has to be power-cycled
  if not assigned(AGetter) then
    Exit;
  GDouble := AGetter();
  if ((abs(ADouble) < 1E-25) and (abs(GDouble-ADouble) > 0.01)) or
     ((abs(ADouble) > 1E-25) and ((abs(GDouble-ADouble) / ADouble) > 0.01)) then
    WriteLn('Error: Set ',ADouble,' ',AUnit,', but got ',GDouble,' ',AUnit,'.');
End;

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
  DA       : TDynDoubleArray;
  MA       : TDynMeasureResultArray;
  Filename : String;
  I        : Integer;
{$IFDEF TEST_WAVEFORM}
  Waveform : TWaveform;
  W        : Word;
{$ENDIF}

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

Function LongRecordChecker(Data:Pointer) : Boolean;
Var OSC : Integer;
Begin
  OSC := MSOX.GetOperationStatusCondition;
  Write('$',IntToHex(OSC, 4),' ');
  Result := ((OSC and COpStatRun) = 0);
End;

Begin
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

  WriteLn('Test program demonstrating remote control of the Agilent InfiniiVision MSO-X 3000A scopes');
  WriteLn;
  WriteLn('Identity: ',MSOX.Identify);
  WriteLn('Current date at scope: ',MSOX.GetDateTime);
  WriteLn;

  // setup to display test signal (0mV to 600mV, 1kHz)
  WriteLn('Setup for test signal');
  MSOX.Reset;
  Sleep(1000);
  MSOX.Stop;   // stop acquisition, because it disrupts the setup commands below
  MSOX.SetTimebaseMode(tmMain);
  MSOX.Run;
  MSOX.Channel[CH2].Display(false);  // switch off channel 2
  MSOX.Channel[CH3].Display(false);  // switch off channel 3
  MSOX.Channel[CH4].Display(false);  // switch off channel 4
  MSOX.Channel[CH1].SetCoupling(cpDC);
  MSOX.Channel[CH1].SetBWLimit(True);
  MSOX.Channel[CH1].SetVDiv(0.5);  // 0.5V/div
  WriteLn('Bandwidth limit: ',Select(MSOX.Channel[CH1].GetBWLimit, 'on', 'off'));

{$IFDEF TEST_TDIV}
  WriteLn('Testing time base');
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 50.0);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 20.0);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 10.0);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  5.0);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  2.0);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  1.0);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',500.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',200.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',100.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 50.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 20.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 10.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  5.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  2.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  1.0E-3);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',500.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',200.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',100.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 50.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 20.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 10.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  5.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  2.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  1.0E-6);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',500.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',200.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',100.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 50.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 20.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div', 10.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  5.0E-9);
  TestSetting(@MSOX.SetTDiv,@MSOX.GetTDiv,'s/div',  2.0E-9);
{$ENDIF TEST_TDIV}
{$IFDEF TEST_TRIGGER_LEVEL}
  WriteLn('Testing trigger level');
  MSOX.Channel[CH1].SetVDiv(0.2);  // 0.2V/div
  For I := -10 to 10 do
    TestSetting(@MSOX.SetTriggerLevel,@MSOX.GetTriggerLevel,'V', I * 0.1);
{$ENDIF TEST_TRIGGER_LEVEL}
{$IFDEF TEST_VDIV}
  WriteLn('Testing vertical sensitivity');
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div', 50.0);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div', 20.0);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div', 10.0);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div',  5.0);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div',  2.0);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div',  1.0);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div',500.0E-3);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div',200.0E-3);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div',100.0E-3);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div', 50.0E-3);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div', 20.0E-3);
  TestSetting(@MSOX.Channel[CH1].SetVDiv,@MSOX.Channel[CH1].GetVDiv,'V/div', 10.0E-3);
{$ENDIF TEST_VDIV}
{$IFDEF TEST_VOFST}
  WriteLn('Testing vertical offset');
  MSOX.Channel[CH1].SetVDiv(0.2);  // 0.2V/div
  For I := -20 to 20 do
    TestSetting(@MSOX.Channel[CH1].SetOffset,@MSOX.Channel[CH1].GetOffset,'V', I * 0.5);  // +/-10V at 100mV/div to 500mV/div
{$ENDIF TEST_VOFST}
{$IF defined(TEST_MEASURE) or defined(TEST_SCREENSHOT) or defined(TEST_WAVEFORM)}
  WriteLn('Setup for next tests');
  MSOX.Channel[CH1].SetVDiv(0.5);  // 0.5V/div
  MSOX.Channel[CH1].SetOffset(1.0);   // set base line to 1.0V = 2 div below center
  MSOX.SetTDiv(0.0005);  // 500us/div
  MSOX.SetTriggerSource(tsCH1);
  MSOX.SetTriggerType(ttEdge);
  MSOX.SetTriggerSlope(tsPositive);
  MSOX.SetTriggerLevel(1.25);   // at 1.0V (half of 2.5Vpp signal)

  // start acquisition
  MSOX.SetTriggerMode(tmNormal);

  WriteLn('Please prepare the test by connecting the probe of channel 1 to the');
  WriteLn('square wave calibration output. Then press [Enter] to save a');
  Write  ('screenshot.');
  ReadLn;
{$ENDIF}
{$IFDEF TEST_MEASURE}
  WriteLn;
  WriteLn('Testing measurements');
  // measurement: Amplitude, Frequency, Duty Cycle, Rise Time
  MSOX.MeasureClear;
  MSOX.MeasureAdd(mtVAmplitude, msCH1);
  MSOX.MeasureAdd(mtFrequency,  msCH1);
  MSOX.MeasureAdd(mtDutyCycle,  msCH1);
  MSOX.MeasureAdd(mtRisetime,   msCH1);
  // clear statistics, all four start with 0 counts
  MSOX.MeasureStatisticsReset;
  // select Mean as statistics type for GetMeasureResults, all other statistics
  // will still be collected
  MSOX.SetMeasureStatistics(stMean);
  Sleep(2500);

  WriteLn('Current statistics type is ' + GetEnumName(TypeInfo(TStatisticsType), Ord(MSOX.GetMeasureStatistics)));
  DA := MSOX.GetMeasureResults(stMean);
  WriteLn('Measurement results:');
  WriteLn('  Mean Amplitude  = ',DA[0]:1:3,' V');
  WriteLn('  Mean Frequency  = ',DA[1]*1E-3:1:3,' kHz');
  WriteLn('  Mean Duty Cycle = ',DA[2]:1:3,' %');
  WriteLn('  Mean Rise Time  = ',DA[3]*1E6:1:3,' µs');
  WriteLn;

  MSOX.SetMeasureStatistics(stAll);
  WriteLn('Current statistics type is ' + GetEnumName(TypeInfo(TStatisticsType), Ord(MSOX.GetMeasureStatistics)));
  MA := MSOX.GetMeasureResults;
  WriteLn('Measurement results:');
  For I := 0 to Length(MA)-1 do
    Begin
      WriteLn(MA[I].Name,':');
      WriteLn('  Current = ',MA[I].Current:1:5);
      WriteLn('  Min     = ',MA[I].Min    :1:5);
      WriteLn('  Max     = ',MA[I].Max    :1:5);
      WriteLn('  Mean    = ',MA[I].Mean   :1:5);
      WriteLn('  StdDev  = ',MA[I].StdDev :1:5);
      WriteLn('  Count   = ',MA[I].Count);
    End;
{$ENDIF}
{$IFDEF TEST_LONG_RECORD}
  WriteLn;
  WriteLn('Testing long-running recording at 1s/dev');
  MSOX.SetTDiv(1.0);              // record at 1s/div --> 10s for one recording
  MSOX.MeasureStatisticsReset;
  MSOX.Single;
  MSOX.TriggerForce;              // force trigger, because trigger mode is always "normal" in single shot mode
  // wait for the scope to finish
  I := WaitTimeout(5000, 100, 15000, @LongRecordChecker, Nil);
  WriteLn;
  if I >= 15000 then
    WriteLn('Timeout!!!')
  else
    WriteLn('Done after ',I,' ms.');
{$ENDIF TEST_LONG_RECORD}
{$IFDEF TEST_SCREENSHOT}
  WriteLn;
  WriteLn('Testing screenshot');

  // save a screenshot
  Filename := 'MSOX-'+FormatDateTime('yyyymmdd-hhnnss',Now)+'.png';
  WriteLn('Saving screenshot to ',Filename);
  MSOX.SetHardcopyOptions(false);  // inksaver off: image is not inverted, i.e., mostly black
  WriteLn('  Hardcopy Options: ',MSOX.GetHardcopyOptions);
  WriteData(Filename,MSOX.Screen(ifPng, ipColor));
{$ENDIF TEST_SCREENSHOT}
{$IFDEF TEST_WAVEFORM}
  WriteLn;
  WriteLn('Testing downloading waveform data with normal settings');
  MSOX.MeasureStatisticsReset;
  MSOX.Single;
  MSOX.TriggerForce;              // force trigger, because trigger mode is always "normal" in single shot mode
  Sleep(100);
  MSOX.SetWaveformFormat(wfByte);
  MSOX.SetWaveformPointsMode(wpmRaw);
  MSOX.SetWaveformPointsCount(100);
//  MSOX.SetWaveformPointsCount(0);
  Waveform := MSOX.GetWaveformPreamble;
  Waveform.PrintPreamble;
  MSOX.GetWaveformData(Waveform);
  Waveform.ConvToReal;
  Waveform.ConvTimes;
  Waveform.PrintAsciiArt(80, 25, 500e-6, 0.5);
  Waveform.Free;
  // try again with 16 bit values
  WriteLn;
  WriteLn('Testing downloading waveform data with High Res acquisition');
  MSOX.SetAcquireType(atHighRes);
//  MSOX.SetAcquireType(atAverage);
//  MSOX.SetAcquireCount(2);
  WriteLn('Acquire Type = ', MSOX.GetAcquireType);
  // With AcquireType = atNormal,                the data is quantized with $0100, i.e., still only 8 bits despite word data selected below.
  // With AcquireType = atHighRes and atAverage, the data is quantized with $0010, i.e., 16x higher resolution.
  MSOX.MeasureStatisticsReset;
  MSOX.Single;
  MSOX.TriggerForce;              // force trigger, because trigger mode is always "normal" in single shot mode
  Sleep(100);
  MSOX.SetWaveformFormat(wfWord);
  MSOX.SetWaveformPointsMode(wpmNormal);
  MSOX.SetWaveformPointsCount(100);       // interestingly, with these settings only 99 points are transferred and XIncrement is 50.206µs to compensate, setting count to 101 crashes the scope
  Waveform := MSOX.GetWaveformPreamble;
  Waveform.PrintPreamble;
  MSOX.GetWaveformData(Waveform);
  // OR all waveform data bits on top of each other to check quantization
  W := 0;
  For I := 0 to Length(Waveform.FWordData)-1 do
    W := W or Waveform.FWordData[I];
  WriteLn('Waveform data bits used: $', IntToHex(W, 4)); // $FFF0 with atHighRes and $FF00 with atNormal (often not all upper bits set)
  // convert, draw, and save
  Waveform.ConvToReal;
  Waveform.ConvTimes;
  Waveform.PrintAsciiArt(80, 25, 500e-6, 0.5);
  Filename := 'MSOX-waveform-'+FormatDateTime('yyyymmdd-hhnnss',Now)+'.csv';
  WriteLn('Saving waveform to ',Filename);
  Waveform.SaveCSV(Filename);
  Waveform.Free;
  // TODO: properly implement atPeak, see [PG] p. 961
{$ENDIF TEST_WAVEFORM}

  MSOX.Free;

  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USBTMC}
  Context.Free;
{$ENDIF USBTMC}
End.

