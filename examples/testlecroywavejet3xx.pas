(**
 * Demonstrate remote control of the LeCroy WaveJet oscilloscopes
 *
 * Important: The WaveJet 300A series USB port does _not_ implement USB-TMC!
 * There is a Windows driver which implements a virtual COM port.
 *
 * This test program can either communicate via USB or ethernet. Define either
 * USB or TCP.
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
Program TestLeCroyWaveJet3xx;

{$mode objfpc}{$H+}

// either communicate via USB or TCP, define one of these two
{ $ DEFINE USB}
{$DEFINE TCP}

// define 0-n of the following to enable/disable the respective tests
{ $ DEFINE TEST_TDIV}
{ $ DEFINE TEST_TRIGGER_LEVEL}
{ $ DEFINE TEST_VDIV}
{ $ DEFINE TEST_VOFST}
{$DEFINE TEST_SCREENSHOT}

Uses
  Classes, SysUtils, PasGpibUtils,
{$IFDEF USB}
  LibUsbOop, USBLeCroy, DevComUSBLeCroy,
{$ENDIF USB}
{$IFDEF TCP}
  DevComTCPLeCroy,
{$ENDIF TCP}
  LeCroyWaveJet;

Const
{$IFDEF USB}
  idVendor  = $05FF;
  idProduct = $0021;
  bConfig   = 1;
  bIntfNum  = 0;
  bAltSet   = 0;
{$ENDIF USB}
{$IFDEF TCP}
  Host = '128.131.80.224';//'128.131.81.97';
  Port = 1861;
{$ENDIF TCP}

Type
  TSetter = Procedure(ADouble:Double) of object;
  TGetter = Function : Double of object;

Procedure TestSetting(ASetter:TSetter;AGetter:TGetter;AUnit:String;ADouble:Double);
Var GDouble : Double;
Begin
  WriteLn('Testing ',ADouble,' ',AUnit);
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
{$IFDEF USB}
  Context  : TLibUsbContext;
  Dev      : TUSBLeCroy;
  Comm     : TUSBLeCroyCommunicator;
{$ENDIF USB}
{$IFDEF TCP}
  Comm     : TTCPLeCroyCommunicator;
{$ENDIF TCP}
  WJ       : TLeCroyWaveJet;
  Filename : String;
  I        : Integer;

Begin
  { device communicator }
{$IFDEF USB}
  // device connector via USB
  Context := TLibUsbContext.Create;

  Dev := TUSBLeCroy.Create(Context,idVendor,idProduct,bConfig,bIntfNum,bAltSet);
  Comm := TUSBLeCroyCommunicator.Create(Dev);
{$ENDIF USB}
{$IFDEF TCP}
  // device connector via TCP/IP with special LeCroy headers
  Comm := TTCPLeCroyCommunicator.Create(Host,Port);
{$ENDIF TCP}
  Comm.SetTimeout(5000000{us});
  { remote instrument }
  WJ := TLeCroyWaveJet.Create(Comm);

  WriteLn('Test program demonstrating remote control of the LeCroy WaveJet scopes');
  WriteLn;
  WriteLn('Current date at scope: ',WJ.GetDate);
  WriteLn;

  // setup to display test signal (0mV to 600mV, 1kHz)
  WriteLn('Setup for test signal');
  WJ.Reset;
  Sleep(1000);
  WJ.SetTriggerMode(tmStop);   // stop acquisition, because it disrupts the setup commands below
  WJ.SetDisplayType(dtYT);
  WJ.Channel[CH2].Trace(false);  // switch off channel 2
  WJ.Channel[CH3].Trace(false);  // switch off channel 3
  WJ.Channel[CH4].Trace(false);  // switch off channel 4
  WJ.Channel[CH1].SetCoupling(cpDC1M);

{$IFDEF TEST_TDIV}
  WriteLn('Testing time base');
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 50.0);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 20.0);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 10.0);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  5.0);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  2.0);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  1.0);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',500.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',200.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',100.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 50.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 20.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 10.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  5.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  2.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  1.0E-3);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',500.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',200.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',100.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 50.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 20.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 10.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  5.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  2.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  1.0E-6);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',500.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',200.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',100.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 50.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 20.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div', 10.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  5.0E-9);
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  2.0E-9);    // WJ354/352/334/332/324/322 only
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',  1.0E-9);    // WJ354/352/334/332 only
  TestSetting(@WJ.SetTDiv,@WJ.GetTDiv,'s/div',500.0E-12);   // WJ354/352 only
{$ENDIF TEST_TDIV}
{$IFDEF TEST_TRIGGER_LEVEL}
  WriteLn('Testing trigger level');
  WJ.Channel[CH1].SetVDiv(0.2);  // 0.2V/div
  For I := -10 to 10 do
    TestSetting(@WJ.SetTriggerLevel,@WJ.GetTriggerLevel,'V', I * 0.1);
{$ENDIF TEST_TRIGGER_LEVEL}
{$IFDEF TEST_VDIV}
  WriteLn('Testing vertical sensitivity');
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',100.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div', 50.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div', 20.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div', 10.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',  5.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',  2.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',  1.0);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',500.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',200.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',100.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div', 50.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div', 20.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div', 10.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',  5.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',  2.0E-3);
  TestSetting(@WJ.Channel[CH1].SetVDiv,@WJ.Channel[CH1].GetVDiv,'V/div',  1.0E-3);
{$ENDIF TEST_VDIV}
{$IFDEF TEST_VOFST}
  WriteLn('Testing vertical offset');
  WJ.Channel[CH1].SetVDiv(0.2);  // 0.2V/div
  For I := -20 to 20 do
    TestSetting(@WJ.Channel[CH1].SetOffset,@WJ.Channel[CH1].GetOffset,'V', I * 0.5);  // +/-10V at 100mV/div to 500mV/div
{$ENDIF TEST_VOFST}
{$IFDEF TEST_SCREENSHOT}
  WriteLn('Testing screenshot');
  WJ.Channel[CH1].SetVDiv(0.2);  // 0.2V/div
  WJ.Channel[CH1].SetOffset(-0.4);   // set base line to -0.4V = 2 div below center
  WJ.SetTDiv(0.0005);  // 500us/div
  WJ.SetTriggerSource(tsCH1);
  WJ.SetTriggerType(ttEdge);
  WJ.SetTriggerSlope(true);
  WJ.SetTriggerLevel(0.3);   // at 0.3V (half of 600mV signal)

  // start acquisition
  WJ.SetTriggerMode(tmNormal);

  WriteLn('Please prepare the test by connecting the probe of channel 1 to the');
  WriteLn('square wave calibration output. Then press [Enter] to save a');
  Write  ('screenshot.');
  ReadLn;

  // save a screenshot
  Filename := 'wj-'+FormatDateTime('yyyymmdd-hhnnss',Now)+'.png';
  WriteLn('Saving screenshot to ',Filename);
  WriteData(Filename,WJ.Screen(ifPng));

  WJ.Free;

  Comm.Free;
{$IFDEF TCP}
{$ENDIF TCP}
{$IFDEF USB}
  Dev.Free;
{$ENDIF USB}
End.

