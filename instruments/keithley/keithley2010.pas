Unit Keithley2010;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  DevCom, RemoteInstrument;

Type

  TKeithley2000Scan = (ksNone,ks2000Scan,ks2001Scan,ks2001TCScan);

  { TKeithley2010 }

  (**
   * Keithley 2010 multimeters
   *
   * The 2000-SCAN 10-Ch, General-Purpose, Scanner Card is supported.
   *)
  TKeithley2010 = class(TRemoteInstrument)
  private
    FScanner : TKeithley2000Scan;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    Function Fetch : Double;
    Function FreshData : Double;
    Procedure SelectChannel(Ch: Integer);
    property Scanner : TKeithley2000Scan read FScanner;
  End;

Implementation
Uses PasGpibUtils;

{ TKeithley2010 }

(**
 * Constructor
 *
 * Checks whether the connected device is a Keithley 2010 multimeter and throws
 * an Exception if not. Additionally, it is checked whether a scanner cards
 * is present.
 *)
Constructor TKeithley2010.Create(ADeviceCommunicator: IDeviceCommunicator);
Var Identity : String;
    IdnArr   : TDynStringArray;
    Options  : String;
    OptArr   : TDynStringArray;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  IdnArr := SplitStr(',',Identity);
//  For I := 0 to Length(IdnArr)-1 do
//    WriteLn(I,': ',IdnArr[I]);
  if (Length(IdnArr) <> 4) or
     (IdnArr[0] <> 'KEITHLEY INSTRUMENTS INC.') or
     (IdnArr[1] <> 'MODEL 2010') then
    raise Exception.Create('Device '''+Identity+''' is not a Keithley 2010 Multimeter');
  { check if an optional scanner card is present }
  Options := FDeviceCommunicator.Query('*OPT?');
  // returns e.g. '0,200X-SCAN', which seem to be two options, the first could
  // be a memory extension, the second one a scanner card.
  OptArr := SplitStr(',',Options);
  if Find('200X-SCAN',OptArr) >= 0 then
    FScanner := ks2000Scan;
End;

Destructor TKeithley2010.Destroy;
Begin
  Inherited Destroy;
End;

(**
 * Fetch the latest measurement value
 *
 * This command can return the same measurement multiple times.
 *)
Function TKeithley2010.Fetch: Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query('FETCH?'));
End;

(**
 * Fetch a new measurement value
 *
 * This command will not return the same measurement twice. Instead, a new
 * measurement is initiated. The command will wait until the new value is
 * available.
 *)
Function TKeithley2010.FreshData: Double;
Begin
  Result := StrToFloat(FDeviceCommunicator.Query(':DATA:FRESH?'));
End;

(**
 * Select a channel of the scanner card
 *
 * If no scanner card is present, an Exception will be raised.
 *
 * Note that the scanner card ralais need some settling time after closing the
 * contact, otherwise the measurement value will be over- or underrated. The
 * duration depends on your requirements to the precision.
 *)
Procedure TKeithley2010.SelectChannel(Ch:Integer);
Begin
  if FScanner = ksNone then
    raise Exception.Create('Device has no scanner card');
  if (Ch < 1) or (Ch > 10) then
    raise Exception.Create('Invalid channel '+IntToStr(Ch));
//  FDeviceCommunicator.Send(':ROUTE:OPEN:ALL');
//  Sleep(100);
  FDeviceCommunicator.Send(':ROUTE:CLOSE (@'+IntToStr(Ch)+')');
End;

End.

