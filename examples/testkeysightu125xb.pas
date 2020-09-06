(**
 * Test program for Keysight U125xB Handheld True RMS (OLED) Multimeter
 *
 * This test program communicates with the USB to IR adapter, which is
 * actually an PL2303 USB to UART adapter.
 *
 *)
Program TestKeysightU125xB;

{$mode objfpc}{$H+}

// define 0-n of the following to enable/disable the respective tests
{$DEFINE TEST_INITIAL}
{$DEFINE TEST_SETCONFIG}

Uses
  Classes, SysUtils,
  DevComRS232, Serial,
  KeysightU125xB,
  PasGpibUtils;

Const
  Device = '/dev/ttyUSB0';

Var
  Comm   : TRS232Communicator;
  U125xB : TKeysightU125xB;

  MeasureStatus : TMeasureStatus;
  MeasureConfig : Array[1..3] of TMeasureConfig;
  Value         : Double;

Begin
  // device connector via USB to IR
  Comm := TRS232Communicator.Create(Device,9600,8,NoneParity,0,[]);
  Comm.SetTimeout(10000000);

  U125xB := TKeysightU125xB.Create(Comm);
{$IFDEF TEST_INITIAL}
  WriteLn('Connected to device ',U125xB.Identify);

  MeasureStatus := U125xB.GetMeasureStatus;
  WriteLn(MeasureStatus.ToString);
//  WriteLn(MeasureStatus.ToStringLong);
  WriteLn('Rotary Switch is at ',CRotarySwitch[MeasureStatus.RotarySwitch]);


  MeasureConfig[1] := U125xB.GetMeasureConfig;
  WriteLn('Current measurement configuration of channel 1: ',MeasureConfig[1].ToString);

  MeasureConfig[2] := U125xB.GetMeasureConfig(2);
  WriteLn('Current measurement configuration of channel 2: ',MeasureConfig[2].ToString);

  MeasureConfig[3] := U125xB.GetMeasureConfig(3);
  WriteLn('Current measurement configuration of channel 3: ',MeasureConfig[3].ToString);

  WriteLn('Battery level: ',U125xB.GetBatteryLevel:1:2,'%');
  WriteLn('Version: ',U125xB.GetVersion);
  WriteLn('Last error: ',U125xB.GetError);

  Value := U125xB.Measure;
  WriteLn('Measurement Ch1: ',Value,' = ',FloatToStrSI(Value,FormatSettings),CMeasureQuantityUnitSymbol[MeasureConfig[1].Quantity]);
  Value := U125xB.Measure(3);
  WriteLn('Measurement Ch3: ',Value,' = ',FloatToStrSI(Value,FormatSettings),CMeasureQuantityUnitSymbol[MeasureConfig[3].Quantity]);
{$ENDIF TEST_INITIAL}
{$IFDEF TEST_SETCONFIG}
  While true do
    Begin
      MeasureStatus := U125xB.GetMeasureStatus;
      if MeasureStatus.RotarySwitch = rsmAADCAC then Break;
      WriteLn('WARNING: Set the rotary switch to ',CRotarySwitch[rsmAADCAC],' and plug in the correct terminal');
      WriteLn('Then press Enter');
      ReadLn;
    End;
  MeasureConfig[1] := U125xB.GetMeasureConfig;
  WriteLn('Current measurement configuration of channel 1: ',MeasureConfig[1].ToString);
  U125xB.SetMeasureConfig(mqCurrent, mcDC, 0.5);
  MeasureConfig[1] := U125xB.GetMeasureConfig;
  WriteLn('New     measurement configuration of channel 1: ',MeasureConfig[1].ToString);
  U125xB.SetMeasureConfig(mqCurrent, mcACDC, 0);
  MeasureConfig[1] := U125xB.GetMeasureConfig;
  WriteLn('New     measurement configuration of channel 1: ',MeasureConfig[1].ToString);
{$ENDIF TEST_SETCONFIG}

//  WriteLn('Reset to default settings');
//  U125xB.Reset;

  WriteLn('Done');
  U125xB.Free;

  Comm.Free;
End.

