Unit RohdeSchwarzFSEB;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  DevCom, RemoteInstrument;

Type

  { TRohdeSchwarzFSEB }

  TRohdeSchwarzFSEBImageFormat = (ifWmf,ifPcx);

  (**
   * Control Rohde&Schwarz FSEB spectrum analyzers
   *
   * There are old version with a custom firmware and newer devices with a
   * firmware based on Windows NT. Currently only the older versions are
   * suppored.
   *)
  TRohdeSchwarzFSEB = class(TRemoteInstrument)
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    Destructor  Destroy; override;
    Function Screen(AImageFormat:TRohdeSchwarzFSEBImageFormat;ATitle:String) : String;
    Function QueryMMemoryData(AFilename:String) : String;
    Function Date : String;
  End;

Implementation
Uses PasGpibUtils;

{ TRohdeSchwarzFSEB }

(**
 * Constructor
 *
 * Checks whether the connected device is a Rohde&Schwarz FSE spectrum analyzer
 * and throws an Exception if not.
 *)
Constructor TRohdeSchwarzFSEB.Create(ADeviceCommunicator: IDeviceCommunicator);
Var Identity : String;
    IdnArr   : Array of String;
Begin
  inherited Create(ADeviceCommunicator);
  { check device }
  Identity := Identify;
  // Rohde&Schwarz,<model>,<serial_number>,<firmware-level>
  // <model>: model ID (e.g., FSEB 30)
  // <serial_number>: serial number (e.g., 825829/021)
  // <firmware_level>: release level (e.g. 3.40.2)
  IdnArr := SplitStr(',',Identity);
  if (Length(IdnArr) <> 4) or
     (IdnArr[0] <> 'Rohde&Schwarz') or
     (Copy(IdnArr[1],1,3) <> 'FSE') then
    raise Exception.Create('Device '''+Identity+''' is not a Rohde&Schwarz FSE Spectrum Analyzer');
End;

Destructor TRohdeSchwarzFSEB.Destroy;
Begin
  Inherited Destroy;
End;

(**
 * Acquire a screenshot and return its data
 *
 * The screenshot will be saved to the the disk of the device at C:\HCOPY.WMF
 * or C:\HCOPY.PCX (depending on the image format). Be sure that there is no
 * important file with that name on the disk.
 *
 * The contents of that file will finally be retrieved.
 *
 * The file will not be deleted.
 *)
Function TRohdeSchwarzFSEB.Screen(AImageFormat: TRohdeSchwarzFSEBImageFormat;ATitle:String): String;
Type TImageFormatStrArr = Array[low(TRohdeSchwarzFSEBImageFormat)..high(TRohdeSchwarzFSEBImageFormat)] of String[4];
Const ImageFormat : TImageFormatStrArr =
  ('WMF','PCX');
Begin
  FDeviceCommunicator.Send(':DISPLAY:LOGO OFF');
  FDeviceCommunicator.Send(':HCOPY:DESTINATION1 ''MMEM''');
  FDeviceCommunicator.Send(':MMEMORY:NAME ''C:\HCOPY.'+ImageFormat[AImageFormat]+'''');
  FDeviceCommunicator.Send(':HCOPY:DEVICE:COLOR ON');
  FDeviceCommunicator.Send(':HCOPY:DEVICE:LANGUAGE1 '+ImageFormat[AImageFormat]);
//  FDeviceCommunicator.Send(':HCOPY:DEVICE:PRESET1 ON');
  FDeviceCommunicator.Send(':HCOPY:ITEM:ALL');
  FDeviceCommunicator.Send(':HCOPY:ITEM:LABEL:TEXT '''+ATitle+'''');
  FDeviceCommunicator.Send(':HCOPY:PAGE:DIMENSIONS:FULL');
  FDeviceCommunicator.Send(':HCOPY:IMMEDIATE1');
  FDeviceCommunicator.Send('*WAIT');

  Result := QueryMMemoryData('C:\HCOPY.'+ImageFormat[AImageFormat]);
End;

(**
 * Get the contents of a file on the instrument's disk
 *)
Function TRohdeSchwarzFSEB.QueryMMemoryData(AFilename : String) : String;
Var LLen : Byte;
    Len  : Integer;
Begin
  FDeviceCommunicator.Send(':MMEMORY:DATA? '''+AFilename+'''');
  Result := FDeviceCommunicator.Receive;   // use Receive instead of Query, because Query Trim()s the result
  // Result is '#<llen><len><data>'
  //   <llen> ... 1 char stating the length of <len>
  //   <len> .... length of the data
  LLen := StrToInt(Result[2]);
  Len  := StrToInt(Copy(Result,3,LLen));
  Delete(Result,1,LLen+2);
End;

(**
 * Query the current system date in the instrument
 *)
Function TRohdeSchwarzFSEB.Date: String;
Begin
  Result := FDeviceCommunicator.Query(':SYSTEM:DATE?');
End;

End.

