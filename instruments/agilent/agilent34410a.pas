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
    Procedure GetNextError(Out Code:Integer;Out Text : String);
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
    Procedure SetTriggerSource(TriggerSource:TTriggerSource);
    Procedure Initiate;
    Function  Fetch : Double;
    Function  GetValue : Double;
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
    IdnArr   : TDynStringArray;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
//  WriteLn(Identity);   // e.g. 'Keysight Technologies,34461A,MY12345678,A.02.17-02.40-02.17-00.52-04-02'
  IdnArr := SplitStr(',',Identity);
//  For I := 0 to Length(IdnArr)-1 do
//    WriteLn(I,': ',IdnArr[I]);
  if not IsSupportedDevice(IdnArr) then
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

Procedure TAgilent34410A.GetNextError(Out Code : Integer; Out Text : String);
Var St : String;
    I  : Integer;
Begin
  St := GetNextError;   // returns e.g. '-410,"Query INTERRUPTED"' or '+0,"No error"'
  I := Pos(',',St);
  if I = 0 then
    Begin
      Code := StrToInt(St);
      Text := '';
      // raise Exception.Create('Cannot parse return value of GetNextError '''+St+'''');
    End
  else
    Begin
      Code := StrToInt(Copy(St,1,I-1));
      Text := Copy(St,I+2,Length(St)-I-2);
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
 *
 * [UG-34410A] p. 51f
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
 *)
Procedure TAgilent34410A.SetSampleSource(SampleSource : TSampleSource);
Begin
  FDeviceCommunicator.Send('SAMPLE:SOURCE '+CSampleSource[SampleSource]);
End;

(**
 * Set the sampling timer interval.
 *)
Procedure TAgilent34410A.SetSampleTimer(SampleTimer : Double);
Begin
  FDeviceCommunicator.Send('SAMPLE:TIMER '+FloatToStrF(SampleTimer,ffExponent,12,2));
End;

(**
 * Query the sampling timer interval.
 *
 * Up to 3600 sec in 20 μs steps
 *)
Function TAgilent34410A.GetSampleTimer : Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('SAMPLE:TIMER?'));
End;

(**
 * Set the sampling timer interval to its minimum or maximum value.
 *
 * The minimum value depends on the measurement settings.
 *)
Procedure TAgilent34410A.SetSampleTimer(SampleTimer : TMinMax);
Begin
  FDeviceCommunicator.Send('SAMPLE:TIMER '+CMinMax[SampleTimer]);
End;

(**
 * Set the number of samples taken after a trigger
 *
 * Allowed values: 1 - 50000 (34410A), 1 - 1000000 (34411A/L4411A)
 *)
Procedure TAgilent34410A.SetSampleCount(Count : Integer);
Begin
  FDeviceCommunicator.Send('SAMPLE:COUNT '+IntToStr(Count));
End;

(**
 * Select trigger source.
 *
 * stInternal is only supported by 34411A and L4411A, but not by 34410A.
 *)
Procedure TAgilent34410A.SetTriggerSource(TriggerSource : TTriggerSource);
Begin
  FDeviceCommunicator.Send('SAMPLE:SOURCE '+CTriggerSource[TriggerSource]);
End;

(**
 * Place the trigger system from "idle" to "wait-for-trigger" and delete all
 * readings from the memory.
 *)
Procedure TAgilent34410A.Initiate;
Begin
  FDeviceCommunicator.Send('INITIATE');
End;

(**
 * Fetch the latest measurement value
 *
 * This command will wait until the measurement is complete. Then it can return
 * the same measurement multiple times.
 *)
Function TAgilent34410A.Fetch: Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('FETCH?'));
  // TODO: check what happens if SetSampleCount > 1
End;

(**
 * Query the Count oldest readings and delete then from memory.
 *
 * If Count is larger than the number of stored readings, the device generates
 * an error.
 *)
Function TAgilent34410A.DataRemove(Count : Integer) : TDynDoubleArray;
Var St : String;
Begin
  // TODO: set maximum number of bytes to read for TUSBTMC
  St := FDeviceCommunicator.Query('DATA:REMOVE? '+IntToStr(Count));
  Result := SplitDouble(',',St);  // TODO: check
End;

(**
 * Start a measurement of Count samples
 *
 * Either calculate the duration of the total measurements or use *OPC to enable
 * the command to set the "Operation Complete" and "*ESR?" to query this bit,
 * or STATUS:OPERATION:CONDITION? if the device is still measuring, or
 * DATA:POINTS? to query the number of points in the measurement memory.
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

