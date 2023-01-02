Unit RemoteInstrument;

{$mode objfpc}{$H+}
{$MODESWITCH NestedProcVars}

Interface

Uses
  Classes, SysUtils,
  PasGpibUtils,
  Instrument,
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
  TRemoteInstrument = class(TInstrument)
  protected
    FDeviceCommunicator : IDeviceCommunicator;
    Function QueryLong  (Cmd : String) : String;
    Function QueryBinary(Cmd : String) : TDynByteArray;
  public
    Constructor Create(ADeviceCommunicator:IDeviceCommunicator);
    /// Destructor; does not free the device communicator object
    Destructor  Destroy; override;
    Function Identify : String; virtual;
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

Function TRemoteInstrument.QueryLong(Cmd : String) : String;
Var St : String;

  // function to be called by WaitTimeout multiple times until enough data was recieved or a timeout has happened
  Function ReceiveData(Data:Pointer) : Boolean;
  Begin
//    WriteLn('QueryLong at ',Length(St));
    St := St + FDeviceCommunicator.Receive;
//    WriteLn('  and now ',Length(St));
    Result := (St[Length(St)] = #10);
  End;

Begin
  FDeviceCommunicator.Send(Cmd);
  // The return format is '<ascii>'+LF.
  St := '';
  WaitTimeout(0, 1, (FDeviceCommunicator.GetTimeout div 1000)+30000, @ReceiveData, Nil);
  // check
  //WriteLn('Received '+IntToStr(Length(St))+' bytes');
  if St[Length(St)] <> #10 then
    raise Exception.Create('Received invalid data format');
  //Dump(St[1], Length(St));
  SetLength(St, Length(St)-1);
  Result := St;
End;

(**
 * Query command which expects IEEE-488.2 binary block data
 *
 *)
Function TRemoteInstrument.QueryBinary(Cmd : String) : TDynByteArray;
Var St : String;
    L  : Integer;

  // function to be called by WaitTimeout multiple times until enough data was recieved or a timeout has happened
  Function ReceiveData(Data:Pointer) : Boolean;
  Begin
//    WriteLn('QueryBinary at ',Length(St),', L = ',L);
    St := St + FDeviceCommunicator.Receive;
    if (L = 0) and (Length(St) >= 10) then
      Begin
        // received enough to have the full length information
        if (St[1] <> '#') or (St[2] <> '8') then
          raise Exception.Create('Received invalid data format');
        L := StrToInt(Copy(St,3,8));
      End;
    Result := (Length(St) >= 10+L+1);
  End;

Begin
  // not using FDeviceCommunicator.Query because firstly it is limited by the
  // USBTMC (and perhaps others) transfer size, and secondly because it trims
  // the received string.
  FDeviceCommunicator.Send(Cmd);
  (* The return format is '#8<byte_length><binary_block>'+LF
   * where <byte_length> are 8 digits (with leading zeros) specifying the number
   * of bytes in <binary_block>.
   *)
  St := '';
  L  := 0;
  WaitTimeout(0, 1, (FDeviceCommunicator.GetTimeout div 1000)+30000, @ReceiveData, Nil);
  // check
  //WriteLn('Received '+IntToStr(Length(St))+' bytes, expected '+IntToStr(10+L+1)+' ('+IntToStr(L)+' for image data)');
  if Length(St) <> 10+L+1 then
    raise Exception.Create('Received '+IntToStr(Length(St))+' bytes but expected '+IntToStr(10+L+1)+' ('+IntToStr(L)+' for data)');
  if St[Length(St)] <> #10 then
    raise Exception.Create('Received invalid data format');
  //Dump(St[1], Length(St));
  // just use the data starting at the 11th byte
  SetLength(Result,L);
  Move(St[11],Result[0],L);
End;

End.
