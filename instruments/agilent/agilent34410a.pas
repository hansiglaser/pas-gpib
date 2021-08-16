(**
 * Remote control of Agilent/Keysight 3441xA and 3446xA/3447xA Multimeters
 *
 * [UG-34410A]  Agilent 34410A/11A 6 1⁄2 Digit Multimeter User’s Guide
 *              (includes the L4411A 1U DMM), Fifth Edition. June 2012
 *
 * [OSG-34460A] Keysight Truevolt Series Digital Multimeters Operating and
 *              Service Guide, 18 August 2017 Edition 5
 *)
Unit Agilent34410A;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils, DevCom, RemoteInstrument;

Type

  TInputTerminalsSetting = (itFront,itRear);
  TQuantity = (qtVoltageDC,qtVoltageAC,qtCurrentDC,qtCurrentAC,
               qtResistance,qtFResistance,qtContinuity,qtDiode,
               qtCapacitance,qtTemperature,qtFrequency,qtPeriod);
  TSampleSource = (ssImmediate,ssTimer);
  TMinMax = (mmMin,mmMax);
  TMinMaxDef = (mmdMin,mmdMax,mmdDefault);
  TTriggerSource = (tsImmediate,tsExternal,tsBus,tsInternal);

Const
  CInputTerminalsSettingNice : Array[TInputTerminalsSetting] of String = ('Front','Rear');
  CQuantityStr : Array[TQuantity] of String = ('VOLTAGE:DC','VOLTAGE:AC','CURRENT:DC','CURRENT:AC',
                                               'RESISTANCE','FRESISTANCE','CONTINUITY','DIODE',
                                               'CAPACITANCE','TEMPERATURE','FREQUENCY','PERIOD');
  CSampleSource : Array[TSampleSource] of String = ('IMMEDIATE','TIMER');
  CMinMax : Array[TMinMax] of String = ('MIN','MAX');
  CMinMaxDef : Array[TMinMaxDef] of String = ('MIN','MAX','DEF');
  CTriggerSource : Array[TTriggerSource] of String = ('IMMEDIATE','EXTERNAL','BUS','INTERNAL');

Type

  { TAgilent34410A }

  (**
   * Agilent 34410A digital multimeters
   *
   * This is also compatible with 34411A and L4411A.
   *
   * If communicating via TCP/IP, the Agilent 34410A digital multimeters use
   *  - Port 5024 for SCPI Telnet
   *  - Port 5025 for SCPI Socket
   *
   *)
  TAgilent34410A = class(TRemoteInstrument)
  private
    FIdentity : TDynStringArray;
    FQuantity : TQuantity;
    Function GetSenseFunctionStr : String;
  protected
    { internal functions }
    // check if *IDN? string is a supported device
    Function  IsSupportedDevice(IdnArr:TDynStringArray) : Boolean; virtual;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    { device function }
    Procedure Reset;
    Procedure SetBeeper(Enable:Boolean);
    Function  GetNextError : String;
    Procedure GetNextError(Out Code:Integer;Out Msg : String);
    Function  GetInputTerminalsSetting : TInputTerminalsSetting;
    Procedure SetSenseFunction(Quantity:TQuantity);
    Procedure SetNPLC(IntegrationTime:Double);
    Function  GetNPLC : Double;
    Procedure SetAperture(IntegrationTime:Double);
    Function  GetAperture : Double;
    Procedure AutoZero;
    Procedure SetAutoZero(Enable:Boolean);
    Procedure AutoRange;
    Procedure SetAutoRange(Enable:Boolean);
    Procedure SetRange(Range:Double);
    Function  GetRange : Double;
    Procedure SetSampleSource(SampleSource:TSampleSource);
    Procedure SetSampleTimer(SampleTimer:Double);
    Function  GetSampleTimer : Double;
    Procedure SetSampleTimer(SampleTimer:TMinMax);
    Procedure SetSampleCount(Count:Integer);
    Procedure SetTriggerDelay(Delay:Double);
    Function  GetTriggerDelay:Double;
    Procedure SetTriggerSource(TriggerSource:TTriggerSource);
    Procedure Initiate;
    Procedure Abort;
    Function  Fetch : Double;
    Function  FetchAll : TDynDoubleArray;
    Function  GetValue : Double;
    Function  GetNumDataPoints : Integer;
    Function  DataRemove(Count : Integer) : TDynDoubleArray;
    Procedure StartMeasurements(Count:Integer);
  End;

Implementation

{ TAgilent34410A }

(**
 * Constructor
 *
 * Checks whether the connected device is a Agilent 34410A digital multimeter
 * and throws an Exception if not.
 *)
Constructor TAgilent34410A.Create(ADeviceCommunicator: IDeviceCommunicator);
Var Identity : String;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  FIdentity := SplitStr(',',Identity);
  if not IsSupportedDevice(FIdentity) then
    raise Exception.Create('Device '''+Identity+''' is not a supported device');
End;

Destructor TAgilent34410A.Destroy;
Begin
  Inherited Destroy;
End;

Function TAgilent34410A.IsSupportedDevice(IdnArr : TDynStringArray) : Boolean;
Begin
  Result := (Length(IdnArr) = 4) and
            ((IdnArr[0] = 'Agilent Technologies')  and (IdnArr[1] = '34410A')) or
            ((IdnArr[0] = 'Keysight Technologies') and (IdnArr[1] = '34461A'));
End;

(**
 * Reset to default settings
 *
 * This is taken from the page "Power-On and Reset State" in the
 * Agilent 34410A/11A/L4411A 6½ Digit Multimeter Programmer's Reference
 *
 * Measurement Configuration        Default Setting
 *   Function                       DC Volts
 *   Range                          Autorange (for all functions)
 *   Resolution                     6 1/2 Digits (0.3 ppm x Range)*
 *   Integration Time               NPLC On, 1 PLC*
 *   Autozero                       On*
 *   Aperture                       Off, 1 second*
 *   AC Input Filter (bandwidth)    20 Hz (medium filter)
 *   Nulls (individual functions)   Off, 0 (for all measurement functions)
 *   Second Display                 Off
 * * For all DC measurement functions.
 *
 * Math Operations                  Factory Setting
 *   Math State                     Off
 *   Math Registers                 Cleared (all registers)
 *   dB Relative Value              0
 *   dBm Reference Resistance       600 Ohm
 *
 * Triggering Operations            Factory Setting
 *   Trigger Count                  1
 *   Trigger Source                 Immediate
 *   Trigger Delay                  Auto Delay
 *   Sample Count                   1
 *   Sample Source                  Auto
 *   Sample Timer                   1 second
 *
 * System-Related Operations        Factory Setting
 *   Display State (34410A/11A)     On
 *   Reading Memory                 Cleared
 *
 *)
Procedure TAgilent34410A.Reset;
Begin
  FDeviceCommunicator.Send('*RST');
End;

(**
 * Enable/disable the beeper
 *)
Procedure TAgilent34410A.SetBeeper(Enable : Boolean);
Begin
  FDeviceCommunicator.Send('SYSTEM:BEEPER:STATE ' + Select(Enable,'ON','OFF'));
End;

Function TAgilent34410A.GetNextError : String;
Begin
  Result := FDeviceCommunicator.Query('SYSTEM:ERROR?');
End;

Procedure TAgilent34410A.GetNextError(Out Code : Integer; Out Msg : String);
Var St : String;
    I  : Integer;
Begin
  St := GetNextError;   // returns e.g. '-410,"Query INTERRUPTED"' or '+0,"No error"'
  I := Pos(',',St);
  if I = 0 then
    Begin
      Code := StrToInt(St);
      Msg  := '';
      // raise Exception.Create('Cannot parse return value of GetNextError '''+St+'''');
    End
  else
    Begin
      Code := StrToInt(Copy(St,1,I-1));
      Msg  := Copy(St,I+2,Length(St)-I-2);
    End;
End;

(**
 * Query the current state of the input terminal switch
 *)
Function TAgilent34410A.GetInputTerminalsSetting : TInputTerminalsSetting;
Var Terminals : String;
Begin
  Terminals := FDeviceCommunicator.Query('ROUTE:TERMINALS?');
  if      Terminals = 'FRON' then Exit(itFront)
  else if Terminals = 'REAR' then Exit(itRear)
  else
    raise Exception.Create('Unknown terminal location '''+Terminals+''' returned from device.');
End;

(**
 * Select the measurment function
 *)
Procedure TAgilent34410A.SetSenseFunction(Quantity : TQuantity);
Begin
  FQuantity := Quantity;
  FDeviceCommunicator.Send('SENSE:FUNCTION "'+GetSenseFunctionStr+'"');
End;

(**
 * Set integration time in number of power-line cycles
 *
 * Allowed values are:
 *   0.006, 0.02, 0.06, 0.2, 1, 2, 10, 100
 * Models 34411A/L4411A additionally allow:
 *   0.001, 0.006
 * Models 34460A/34461A:
 *   0.02, 0.2, 1, 10, 100
 * Models 34465A/34470A (without DIG option) support additionally:
 *   0.02, 0.06, 0.2, 1.0, 10. 100
 * Models 34465A/34470A (with DIG option) support additionally:
 *   0.001, 0.002, 0.006
 *
 * The aperture times for models 3446xA is given at [OSG-34460A] p. 454ff
 *   0.001  0.002  0.006  0.02   0.06  0.2  1.0     10     100
 *   20µs   40µs   100µs  300µs  1ms   3ms  20ms    200ms  2s
 *   20µs   40µs   100µs  300µs  1ms   3ms  16.7ms  167ms  1.67s
 *
 * For models 3446xA, a default delay is inserted before each measurement of
 *  - 100µs for NPLC <= 0.02,
 *  - 130µs for NPLC = 0.06 and 0.2, and
 *  - 160µs for NPLC >= 1
 * for DCV measurements. Other values apply for other ranges, see [OSG-34460A]
 * p. 458ff. Use SetTriggerDelay to change that value.
 *
 * Measurements with the scope of the "VMComp" BNC output of the 34461A shows
 * a 2µs pulse every time a measurement is finished, see [OSG-34460A] p. 461.
 *
 * With a trigger delay set to 0µs and a fixed range, the following periods
 * were measured:
 *  - NPLC = 0.02 --> 908.86 µs
 *  - NPLC = 0.2  -->   3.001ms
 *  - NPLC = 1    -->  20.001ms
 *
 * [UG-34410A] p. 51f
 * [OSG-34460A] p. 384
 *)
Procedure TAgilent34410A.SetNPLC(IntegrationTime : Double);
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':NPLC '+FloatToStrF(IntegrationTime,ffFixed,1,3));
End;

(**
 * Query integration time in number of power-line cycles
 *
 * Allowed values are:
 *   0.006, 0.02, 0.06, 0.2, 1, 2, 10, 100
 * Models 34411A/L4411A additionally allow:
 *   0.001, 0.006
 *
 * [UG-34410A] p. 51f
 *)
Function TAgilent34410A.GetNPLC : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('SENSE:'+GetSenseFunctionStr+':NPLC?'));
End;

(**
 * Set integration time
 *
 * Allowed values are:
 *   100µs to 1s
 * Models 34411A/L4411A:
 *   20µs to 1s
 *
 * [UG-34410A] p. 52f
 *
 * [OSG-34460A] p. 381f Applies only to the 34465A and 34470A.
 *)
Procedure TAgilent34410A.SetAperture(IntegrationTime : Double);
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':APERTURE '+FloatToStrF(IntegrationTime,ffExponent,8,2));
End;

(**
 * Query integration time
 *
 * Allowed values are:
 *   100µs to 1s
 * Models 34411A/L4411A:
 *   20µs to 1s
 *
 * [UG-34410A] p. 52f
 *
 * [OSG-34460A] p. 381f Applies only to the 34465A and 34470A.
 *)
Function TAgilent34410A.GetAperture : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('SENSE:'+GetSenseFunctionStr+':APERTURE?'));
End;

(**
 * Perform an auto zero measurement
 *)
Procedure TAgilent34410A.AutoZero;
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':ZERO:AUTO ONCE');
End;

(**
 * Enable or disable autozero measurements
 *
 * If enabled, the instrument internally disconnects the input signal
 * following each measurement, and takes a zero reading. It then subtracts the
 * zero reading from the preceding reading.
 *
 * [PR-34410A] [SENSe:]VOLTage[:DC]:ZERO:AUTO
 *)
Procedure TAgilent34410A.SetAutoZero(Enable:Boolean);
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':ZERO:AUTO '+Select(Enable,'ON','OFF'));
End;

(**
 * Perform one autorange, then don't change this range again
 *)
Procedure TAgilent34410A.AutoRange;
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':ZERO:AUTO ONCE');
End;

(**
 * Enable or disable autoranging
 *)
Procedure TAgilent34410A.SetAutoRange(Enable : Boolean);
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':RANGE:AUTO '+Select(Enable,'ON','OFF'));
End;

(**
 * Set the measurement range.
 *
 * Allowed values for voltage measurement:
 *   0.1, 1, 10, 100, 1000
 * Allowed values for current measurement:
 *   0.0001, 0.001, 0.01, 0.1, 1, 3
 *)
Procedure TAgilent34410A.SetRange(Range : Double);
Begin
  FDeviceCommunicator.Send('SENSE:'+GetSenseFunctionStr+':RANGE '+FloatToStrF(Range,ffExponent,2,2));
End;

(**
 * Query the measurement range.
 *)
Function TAgilent34410A.GetRange : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('SENSE:'+GetSenseFunctionStr+':RANGE?'));
End;

(**
 * Set whether each sampling is started after a trigger delay time or
 * periodically using the sampling timer.
 *
 * [OSG-34460A] p. 309f: Applies only to the 34465A and 34470A.
 *)
Procedure TAgilent34410A.SetSampleSource(SampleSource : TSampleSource);
Begin
  FDeviceCommunicator.Send('SAMPLE:SOURCE '+CSampleSource[SampleSource]);
End;

(**
 * Set the sampling timer interval.
 *
 * This presumably only works with the 34465A and 34470A.
 *)
Procedure TAgilent34410A.SetSampleTimer(SampleTimer : Double);
Begin
  FDeviceCommunicator.Send('SAMPLE:TIMER '+FloatToStrF(SampleTimer,ffExponent,12,2));
End;

(**
 * Query the sampling timer interval.
 *
 * Up to 3600 sec in 20 μs steps
 *
 * This presumably only works with the 34465A and 34470A.
 *)
Function TAgilent34410A.GetSampleTimer : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('SAMPLE:TIMER?'));
End;

(**
 * Set the sampling timer interval to its minimum or maximum value.
 *
 * The minimum value depends on the measurement settings.
 *
 * This presumably only works with the 34465A and 34470A.
 *)
Procedure TAgilent34410A.SetSampleTimer(SampleTimer : TMinMax);
Begin
  FDeviceCommunicator.Send('SAMPLE:TIMER '+CMinMax[SampleTimer]);
End;

(**
 * Set the number of samples taken after a trigger
 *
 * Allowed values: 1 - 50000 (34410A), 1 - 1000000 (34411A/L4411A)
 *
 * You can store up to 1,000 measurements in the reading memory of the 34460A,
 * 10,000 measurements on the 34461A, 50,000 measurements on the 34465A/70A
 * (without the MEM option), or 2,000,000 measurements on the 34465A/70A (with
 * the MEM option) ([OSG-34460A] p. 307).
 *)
Procedure TAgilent34410A.SetSampleCount(Count : Integer);
Begin
  FDeviceCommunicator.Send('SAMPLE:COUNT '+IntToStr(Count));
End;

(**
 * Set the delay between the trigger signal and the first measurement.
 *
 * This may be useful in applications where you want to allow the input to
 * settle before taking a measurement or for pacing a burst of measurements.
 *
 * If you specify a trigger delay with this command, that delay is used for
 * all functions (except CONTinuity and DIODe) and ranges. The CONTinuity and
 * DIODe tests ignore the trigger delay setting.
 *
 * If you have configured the instrument for more than one measurement per
 * trigger (SAMPle:COUNt>1), the delay is inserted after the trigger and
 * between consecutive measurements.
 *
 * @param Delay  0 to ~3600 seconds (~1 μs steps)
 *
 * [OSG-34460A] p. 96ff, 431
 *)
Procedure TAgilent34410A.SetTriggerDelay(Delay : Double);
Begin
  FDeviceCommunicator.Send('TRIGGER:DELAY '+FloatToStrF(Delay,ffExponent,12,2));
End;

Function TAgilent34410A.GetTriggerDelay : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('TRIGGER:DELAY?'));
End;

(**
 * Select trigger source.
 *
 * stInternal is only supported by 34411A and L4411A, but not by 34410A.
 *
 * [OSG-34460A] p. 435ff:
 *  - The INTernal source is available for the 34465A and 34470A with the DIG
 *    option only.
 *  - On the 34460A, EXTernal requires option 34460-LAN or option 3446LANU.
 *)
Procedure TAgilent34410A.SetTriggerSource(TriggerSource : TTriggerSource);
Begin
  FDeviceCommunicator.Send('TRIGGER:SOURCE '+CTriggerSource[TriggerSource]);
End;

(**
 * Place the trigger system from "idle" to "wait-for-trigger" and delete all
 * readings from the memory.
 *
 * The INITiate command is also an "overlapped" command. This means that after
 * executing INITiate, you can send other commands that do not affect the
 * measurements.
 *
 * Use ABORt to return to idle.
 *
 * [OSG-34460A] p. 204
 *)
Procedure TAgilent34410A.Initiate;
Begin
  FDeviceCommunicator.Send('INITIATE');
End;

(**
 * Abort a measurement in progress and returns the instrument to the trigger
 * idle state.
 *
 * Use this to abort a measurement when the instrument is waiting for a
 * trigger, or for aborting a long measurement or series of measurements.
 *
 * [OSG-34460A] p. 202
 *)
Procedure TAgilent34410A.Abort;
Begin
  FDeviceCommunicator.Send('ABORT');
End;

(**
 * Fetch the latest measurement value
 *
 * This command will wait until the measurement is complete. Then it can return
 * the same measurement multiple times.
 *
 * Don't use this command if a measurement series was performed!
 *)
Function TAgilent34410A.Fetch : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('FETCH?'));
End;

(**
 * Waits for measurements to complete and fetches all values.
 *
 * This command will wait until the measurement (series) is complete. Then it
 * can return the same data multiple times.
 *
 * Use this command also for a measurement series.
 *
 *)
Function TAgilent34410A.FetchAll : TDynDoubleArray;
Var St : String;
Begin
  St := QueryLong('FETCH?');
  Result := SplitDouble(',',St);
End;

(**
 * Query the total number of measurements currently in reading memory.
 *
 *)
Function TAgilent34410A.GetNumDataPoints : Integer;
Begin
  Result := StrToInt(FDeviceCommunicator.Query('DATA:POINTS?'));
End;

(**
 * Query the Count oldest readings and delete then from memory.
 *
 * If Count is larger than the number of stored readings, the device generates
 * an error.
 *
 * See also the documentation of FetchAll for considerations on the USBTMC
 * transfer size.
 *)
Function TAgilent34410A.DataRemove(Count : Integer) : TDynDoubleArray;
Var St : String;
Begin
  St := FDeviceCommunicator.Query('DATA:REMOVE? '+IntToStr(Count));
  Result := SplitDouble(',',St);
End;

(**
 * Start a measurement of Count samples
 *
 * Either calculate the duration of the total measurements or use *OPC to enable
 * the command to set the "Operation Complete" and "*ESR?" to query this bit,
 * or STATUS:OPERATION:CONDITION? if the device is still measuring, or
 * DATA:POINTS? to query the number of points in the measurement memory.
 *
 * FETCH? will wait until the completion of the measurement series.
 *
 * Do not use this function with 34460A and 34461A, because they don't support
 * the sample source and timer functions!
 *)
Procedure TAgilent34410A.StartMeasurements(Count : Integer);
Begin
  // use sample timer to have deterministic delays between samples
  SetSampleSource(ssTimer);
  // use smallest possible delay between samples
  SetSampleTimer(mmMin);
  // set number of samples
  SetSampleCount(Count);
  // don't wait for any trigger condition
  SetTriggerSource(tsImmediate);
  // perform autozero measurement
  AutoZero;
  // initiate measurements
  Initiate;
End;

(**
 * Issue a single measurement and get its value
 *
 * This function initially clears the measurement memory but does not clean up
 * the measured value from the memory afterward.
 *)
Function TAgilent34410A.GetValue : Double;
Begin
  SetSampleCount(1);
  Initiate;
  Result := Fetch;
End;

{ private methods }

Function TAgilent34410A.GetSenseFunctionStr : String;
Begin
  Result := CQuantityStr[FQuantity];
End;

End.

