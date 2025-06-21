(**
 * Test program for Keysight EL30000 DC Electronic Loads
 *
 *)
Program testkeysightel30000;

{$MODE OBJFPC}{$H+}

// select device type, define one of these:
{$DEFINE KeysightEL33133A}
{ $ DEFINE KeysightEL34143A}
{ $ DEFINE KeysightEL34243A}

// either communicate via USB-TMC or TCP, define one of these two
{$DEFINE USBTMC}
{ $ DEFINE TCP}

{$DEFINE TEST_DISPLAY}
{$DEFINE TEST_SENSE_MODE}
{$DEFINE TEST_CURRENT_LOAD}
{$DEFINE TEST_VOLTAGE_LOAD}
{$DEFINE TEST_RESISTANCER_LOAD}
{$DEFINE TEST_POWER_LOAD}
{$DEFINE TEST_MEASURE_ARRAY}
{$DEFINE TEST_TRIG_MEASURE}
{$DEFINE TEST_TRANSIENT}

Uses
  Classes, SysUtils, Math, PasGpibUtils, DevCom,
  DevComVisa,
{$IFDEF USBTMC}
  UsbTmc,DevComUSBTMC,
{$ENDIF USBTMC}
  KeysightEL30000;

Const
{$IFDEF USBTMC}
  Visa = 'USB0::0x2a8d::0x3802::*::INSTR';
{$ELSE}
  Visa = 'TCPIP::10.0.0.203::5025::SOCKET';
{$ENDIF USBTMC}

Var
  Comm          : IDeviceCommunicator;
  CommObj       : TObject;
{$IFDEF USBTMC}
  TmcComm       : TUSBTMCCommunicator;
  Status        : Byte;
{$ENDIF USBTMC}
  EL30000       : TKeysightEL30000;

Procedure ShowMeasure(Ch:TChannel);
Var V,C,P,R : Double;
Begin
  V := EL30000.MeasureVoltage(Ch);
  C := EL30000.MeasureCurrent(Ch);
  P := EL30000.MeasurePower  (Ch);
  if abs(C) > 0.0 then R := V / C
  else R := Infinity;
  WriteLn('  V = ',V:7:3,'V, I = ',C:6:3,'A, P = ',P:7:3,'W, R = ',R:7:3,'Ω');
End;

{$IFDEF TEST_MEASURE_ARRAY}
Procedure ShowMeasureArray(Ch:TChannelSet);
Var V,C,P       : TDynDoubleArray;
    MV,MC,MP,MR : Double;
Begin
  V := EL30000.MeasureArrayVoltage(Ch);
  C := EL30000.MeasureArrayCurrent(Ch);
  P := EL30000.MeasureArrayPower  (Ch);
  MV := Mean(V);
  MC := Mean(C);
  MP := Mean(P);
  if abs(MC) > 0.0 then MR := MV / MC
  else MR := Infinity;
  WriteLn('  V = ',MV:7:3,'V, I = ',MC:6:3,'A, P = ',MP:7:3,'W, R = ',MR:7:3,'Ω');
End;
{$ENDIF TEST_MEASURE_ARRAY}

{$IFDEF TEST_TRIG_MEASURE}
Procedure ShowFetchArray(Ch:TChannelSet);
Var V,C,P,R     : TDynDoubleArray;
    I           : Integer;
    MV,MC,MP,MR : Double;
Begin
  V := EL30000.FetchArrayVoltage(Ch);
  C := EL30000.FetchArrayCurrent(Ch);
  P := EL30000.FetchArrayPower  (Ch);
  if Length(V) = Length(C) then
    Begin
      SetLength(R, Length(V));
      For I := 0 to Length(V)-1 do
        if abs(C[I]) > 0.0 then R[I] := V[I] / C[I]
        else R[I] := Infinity;
    End
  else
    Begin
      WriteLn('Error: The arrays for voltage and current have differing length (',Length(V),' vs. ',Length(C),').');
      SetLength(R, 1);
      R[0] := NaN;
    End;
  MV := Mean(V);
  MC := Mean(C);
  MP := Mean(P);
  MR := Mean(R);
  WriteLn('  V = ',MV:7:3,'V, I = ',MC:6:3,'A, P = ',MP:7:3,'W, R = ',MR:7:3,'Ω');
End;
{$ENDIF TEST_TRIG_MEASURE}

Procedure ErrorHandler;
Var I    : Integer;
    Code : Integer;
    Msg  : String;
Begin
  if not assigned(EL30000) then Exit;    // prevent accessing the device before its constructor has finished
  // print all errors in the queue
  For I := 0 to 20 do       // 34410A and 34461A can store up to 20 errors
    Begin
      {$IFDEF USBTMC}
      if (TmcComm.Device.ReadStatusByte and IEEE488_StatusByte_ErrorQueue) = 0 then Break;
      {$ENDIF}
      EL30000.GetNextError(Code,Msg);
      if Code = 0 then Break;
      if I = 0 then
        WriteLn('Error Queue:');
      WriteLn('  ',Code,': ',Msg);
    End;
End;

Begin
  WriteLn('Contacting Keysight EL30000 Series DC Electronic Load ', Visa);

  Comm := DevComOpen(Visa, CommObj);
  Comm.SetTimeout(10000000);

  EL30000 := TKeysightEL30000.Create(Comm);
  WriteLn('Connected to device ',EL30000.Identify);

  WriteLn('Reset to default settings');
  EL30000.Reset;
  WriteLn('Disable the beeper');
  EL30000.SetBeeper(false);

{$IFDEF USBTMC}
  TmcComm := CommObj as TUSBTMCCommunicator;
  TmcComm.ErrorHandler := @ErrorHandler;
  // check residues from previous commands
  Status := TmcComm.Device.ReadStatusByte;
  if (Status and IEEE488_StatusByte_MessageAvailable) <> 0 then
    WriteLn('Old Reply: ',TmcComm.Receive);
  // print all errors in the queue
  if (Status and IEEE488_StatusByte_ErrorQueue) <> 0 then
    Begin
      WriteLn('There were errors!');
      ErrorHandler;
    End;
{$ELSE}
  // print all errors in queue
  ErrorHandler;
{$ENDIF USBTMC}

{$IFDEF TEST_DISPLAY}
  WriteLn('Testing Display');
  WriteLn('Display is currently '+Select(EL30000.GetDisplayState, 'on', 'off'));
  WriteLn('Turning off');
  EL30000.SetDisplayState(False);
  WriteLn('Display is currently '+Select(EL30000.GetDisplayState, 'on', 'off'));
  Sleep(2000);
  WriteLn('Turning on');
  EL30000.SetDisplayState(True);
  WriteLn('Display is currently '+Select(EL30000.GetDisplayState, 'on', 'off'));
  Sleep(2000);
  WriteLn('Showing text');
  EL30000.SetDisplayText('Hello World!');
  Sleep(2000);
  WriteLn('Clearing the text');
  EL30000.ClearDisplay;
  Sleep(2000);
{$IFDEF KeysightEL34243A} // the only 2-channel instrument
  WriteLn('Showing 1 and 2 channels');
  EL30000.SetDisplayView(1);
  Sleep(2000);
  EL30000.SetDisplayView(2);
  Sleep(2000);
{$ENDIF KeysightEL34243A}
{$ENDIF TEST_DISPLAY}

{$IFDEF TEST_SENSE_MODE}
  WriteLn('The current sense mode is ',CSenseMode[EL30000.GetSenseMode(1)]);
  WriteLn('Setting sense mode to remote (4-wire)');
  EL30000.SetSenseMode   ([1], smRemote);
  Sleep(2000);
  WriteLn('Setting sense mode to local (2-wire)');
  EL30000.SetSenseMode   ([1], smLocal);
  Sleep(2000);
{$ENDIF TEST_SENSE_MODE}

  WriteLn('Connect the instrument to a power supply. Its voltage must be lower than');
  WriteLn('the instrument maximum input voltage. Its current capability must be at');
  WriteLn('least 1.0A and maximum 2.0A');
  WriteLn;
  WriteLn('Press Enter...');
  ReadLn;
{$IFDEF TEST_CURRENT_LOAD}
  WriteLn('Testing constant current load');
  EL30000.SetLoadFunction(1, lfCurrent);
  EL30000.SetCurrentRange(1, 5.0);
  EL30000.SetCurrent     (1, 1.0);
  EL30000.EnableOutput   ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], False);
  Sleep(200);
  EL30000.EnableOutput   ([1], False);
{$ENDIF TEST_CURRENT_LOAD}
{$IFDEF TEST_VOLTAGE_LOAD}
  WriteLn('Testing constant voltage load');
  EL30000.SetLoadFunction(1, lfVoltage);
  EL30000.SetVoltageRange(1, 50.0);
  EL30000.SetVoltage     (1, 5.0);
  EL30000.EnableOutput   ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], False);
  Sleep(200);
  EL30000.EnableOutput   ([1], False);
{$ENDIF TEST_VOLTAGE_LOAD}
{$IFDEF TEST_RESISTANCER_LOAD}
  WriteLn('Testing constant resistance load');
  EL30000.SetLoadFunction   (1, lfResistance);
  EL30000.SetResistanceRange(1, 20.0);
  EL30000.SetResistance     (1, 100.0);
  EL30000.EnableOutput   ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], False);
  Sleep(200);
  EL30000.EnableOutput   ([1], False);
{$ENDIF TEST_RESISTANCE_LOAD}
{$IFDEF TEST_POWER_LOAD}
  WriteLn('Testing constant power load');
  EL30000.SetLoadFunction(1, lfPower);
  EL30000.SetPowerRange  (1, 40.0);
  EL30000.SetPower       (1, 12.0);
  EL30000.EnableOutput   ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], True);
  ShowMeasure(1);
  Sleep(2000);
  EL30000.SetOutputShort ([1], False);
  Sleep(200);
  EL30000.EnableOutput   ([1], False);
{$ENDIF TEST_POWER_LOAD}

{$IFDEF TEST_MEASURE_ARRAY}
  WriteLn('Testing measurement series');
  EL30000.SetLoadFunction (1, lfCurrent);
  EL30000.SetCurrentRange (1, 5.0);
  EL30000.SetCurrent      (1, 1.0);
  EL30000.SetSweepPoints  ([1], 10);
  EL30000.SetSweepInterval([1], 0.1);
  EL30000.EnableOutput    ([1], True);
  ShowMeasureArray        ([1]);
  Sleep(2000);
  EL30000.EnableOutput    ([1], False);
{$ENDIF TEST_MEASURE_ARRAY}

{$IFDEF TEST_TRIG_MEASURE}
  WriteLn('Testing triggered measurement');
  EL30000.SetLoadFunction (1, lfCurrent);
  EL30000.SetCurrentRange (1, 5.0);
  EL30000.SetCurrent      (1, 1.0);
  EL30000.SetSweepPoints  ([1], 10);
  EL30000.SetSweepInterval([1], 0.1);
  EL30000.SenseCurrent    ([1], True);
  EL30000.SenseVoltage    ([1], True);
  EL30000.EnableOutput    ([1], True);
  EL30000.InitiateAcquire ([1]);
  Sleep(100);
  EL30000.TriggerAcquire  ([1]);
  // Be careful! The instrument might hang up itself and not react to any
  // communication any more, of Fetch* are called witout proper triggering.
  Sleep(10*100+100);   // 10 points with an interval of 100ms plus 100ms for good measure
  ShowFetchArray([1]);
  Sleep(2000);
  EL30000.EnableOutput    ([1], False);
{$ENDIF TEST_TRIG_MEASURE}

{$IFDEF TEST_TRANSIENT}
  WriteLn('Testing transient loads');
  EL30000.SetLoadFunction    (1, lfCurrent);
  EL30000.SetCurrentRange    (1, 5.0);
  EL30000.SetCurrent         (1, 0.1);
  EL30000.SetCurrentTransient(1, 1.0);
  EL30000.SetCurrentTransMode(1, stmList);  // using stmStep doesn't work
  // continuous transients, i.e., a square wave
  EL30000.SetTransientMode      ([1], tmContinuous);
  EL30000.SetTransientCount     ([1], 10);
  EL30000.SetTransientDutyCycle ([1], 50.0);
  EL30000.SetTransientFrequency ([1], 10.0);
  EL30000.EnableOutput          ([1], True);
  EL30000.InitiateTransient     ([1]);
  Sleep(100);
  EL30000.TriggerTransient      ([1]);
  Sleep(10*100+100);   // 10 points with an interval of 100ms plus 100ms for good measure
  Sleep(500);
  // a single pulse transient
  EL30000.SetTransientMode      ([1], tmPulse);
  EL30000.SetTransientPulseWidth([1], 0.500);
  EL30000.InitiateTransient     ([1]);
  Sleep(100);
  EL30000.TriggerTransient      ([1]);
  Sleep(500);
  Sleep(500);
  // toggling transient on and of
  EL30000.SetTransientMode      ([1], tmToggle);
  EL30000.InitiateTransientContinuous([1], True);
  Sleep(100);
  EL30000.TriggerTransient      ([1]);
  Sleep(100);
  EL30000.TriggerTransient      ([1]);
  Sleep(100);
  EL30000.TriggerTransient      ([1]);
  Sleep(100);
  EL30000.TriggerTransient      ([1]);
  Sleep(500);
  EL30000.EnableOutput          ([1], False);
{$ENDIF TEST_TRANSIENT}

  // print all errors
  ErrorHandler;

  EL30000.Free;
  CommObj.Free;
End.

