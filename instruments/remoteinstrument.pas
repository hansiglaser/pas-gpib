Unit RemoteInstrument;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  DevCom;

Type

  { TRemoteInstrument }

  (**
   * Base class for remotely connected measurement instruments
   *
   * This base class should be used as parent class for remotely connected
   * measurement instruments. It holds an instance for a IDeviceCommunicator
   * and provides one GPIB/SCPI function Identify which sends '*IDN?'.
   *)
  TRemoteInstrument = class
  protected
    FDeviceCommunicator : IDeviceCommunicator;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    /// Destructor; does not free the device communicator object
    Destructor  Destroy; override;
    Function Identify : String;
  End;

Implementation

{ TRemoteInstrument }

Constructor TRemoteInstrument.Create(ADeviceCommunicator: IDeviceCommunicator);
Begin
  FDeviceCommunicator := ADeviceCommunicator;
End;

Destructor TRemoteInstrument.Destroy;
Begin
  FDeviceCommunicator := Nil;
  Inherited Destroy;
End;

(**
 * Query the device identification string
 *)
Function TRemoteInstrument.Identify: String;
Begin
  Result := FDeviceCommunicator.Query('*IDN?');
End;

End.
