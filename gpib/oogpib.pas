(*
TODO:
 - TGPIB.Read: return to ibrd(), since using a file for every ibrdf() is silly,
   also use a good value for ReadBufferIncrement
*)

Unit OoGPIB;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LinuxGPIB;

Type
  EGPIB = class(Exception);

  { TGPIB }

  TGPIB = class
  private
    FHandle : Integer;
    Procedure CheckStatus(Status:TIBStaBits);
  public
    Constructor Create(ADeviceName:String);
    Destructor Destroy; override;
    { Multidevice API Functions }
(*    Procedure AllSPoll(board_desc:Longint; addressList:array of TAddr4882; resultList:array of smallint);
    Procedure DevClear(board_desc:Longint; address:TAddr4882);
    Procedure DevClearList(board_desc:Longint; addressList:array of TAddr4882);
    Procedure EnableLocal(board_desc:Longint; addressList:array of TAddr4882);
    Procedure EnableRemote(board_desc:Longint; addressList:array of TAddr4882);
    Procedure FindLstn(board_desc:Longint; padList:array of TAddr4882; resultList:array of TAddr4882; maxNumResults:Longint);
    Procedure FindRQS(board_desc:Longint; addressList:array of TAddr4882; var result:smallint);
    Procedure PassControl(board_desc:Longint; address:TAddr4882);
    Procedure PPoll(board_desc:Longint; var result:smallint);
    Procedure PPollConfig(board_desc:Longint; address:TAddr4882; dataLine:Longint; lineSense:Longint);
    Procedure PPollUnconfig(board_desc:Longint; addressList:array of TAddr4882);
    Procedure RcvRespMsg(board_desc:Longint; var buffer:pointer; count:Longint; termination:Longint);
    Procedure ReadStatusByte(board_desc:Longint; address:TAddr4882; var result:smallint);
    Procedure Receive(board_desc:Longint; address:TAddr4882; var buffer:pointer; count:Longint; termination:Longint);
    Procedure ReceiveSetup(board_desc:Longint; address:TAddr4882);
    Procedure ResetSys(board_desc:Longint; addressList:array of TAddr4882);
    Procedure Send(board_desc:Longint; address:TAddr4882; var buffer:pointer; count:Longint; eot_mode:TSendEotMode);
    Procedure SendCmds(board_desc:Longint; var cmds:pointer; count:Longint);
    Procedure SendDataBytes(board_desc:Longint; var buffer:pointer; count:Longint; eotmode:TSendEotMode);
    Procedure SendIFC(board_desc:Longint);
    Procedure SendLLO(board_desc:Longint);
    Procedure SendList(board_desc:Longint; addressList:array of TAddr4882; var buffer:pointer; count:Longint; eotmode:TSendEotMode);
    Procedure SendSetup(board_desc:Longint; addressList:array of TAddr4882);
    Procedure SetRWLS(board_desc:Longint; addressList:array of TAddr4882);
    Procedure TestSRQ(board_desc:Longint; var result:smallint);
    Procedure TestSys(board_desc:Longint; addressList:array of TAddr4882; resultList:array of smallint);
    Function  ThreadIbsta:Longint;
    Function  ThreadIberr:Longint;
    Function  ThreadIbcnt:Longint;
    Function  ThreadIbcntl:Longint;
    Procedure Trigger(board_desc:Longint; address:TAddr4882);
    Procedure TriggerList(board_desc:Longint; addressList:array of TAddr4882);
    Procedure WaitSRQ(board_desc:Longint; var result:smallint);*)
    { Traditional API Functions }
    /// ibask: query configuration (board or device)
    //Function  GetConfig(Option:TIBAskOption):Longint;
    /// ibbna: change access board (device)
    //Procedure SetAccessBoard(BoardName:PChar);
    /// ibcac: assert ATN (board
    //Procedure BecomeActiveController(Synchronous:Boolean);
    /// ibclr: clear device (device)
    //Procedure ClearDevice;
    /// ibcmd: write command bytes (board)
    //Procedure Command(Cmd:String);
    /// ibcmda: write command bytes asynchronously (board)
    //Procedure CommandAsync(Cmd:String);
    /// ibconfig: change configuration (board or device)
    //Procedure SetConfig(Option:TIBConfigOption; Value:Longint);
    /// ibdev: open a device (device)
    //Procedure OpenDevice(BoardIndex:Longint; pad:Longint; sad:Longint; timo:Longint; send_eoi:Longint;eosmode:Longint);
    /// ibdma: ???
    //Function ibdma(v:Longint):Longint;
    /// ibeos: set end-of-string mode (board or device)
    //Procedure SetEOS(v:TEOSFlags);
    /// ibeot: assert EOI with last data byte (board or device)
    //Procedure AssertEOI(EOI:Boolean);
    /// ibevent: get events from event queue (board)
    //Procedure GetEvent : TGpibEvents;
    /// ibfind: open a board or device (board or device)
    Function Find(Name:String) : Integer;
    /// ibgts: release ATN (board)
    //Procedure ibgts(shadow_handshake:Longint);
    /// ibist: set individual status bit (board)
    //Procedure ibist(ist:Longint);
    /// iblines: monitor bus lines (board)
    //Procedure iblines(var line_status:TBusControlLine);
    /// ibln: check if listener is present (board or device)
    //Procedure ibln(pad:Longint; sad:Longint; var found_listener:smallint);
    /// ibloc: go to local mode (board or device)
    //Procedure ibloc:Longint;
    /// ibonl: close or reinitialize descriptor (board or device)
    //Procedure ibonl(onl:Longint);
    /// ibpad: set primary GPIB address (board or device)
    //Procedure ibpad(v:Longint);
    /// ibpct: pass control (board)
    //Procedure ibpct:Longint;
    /// ibppc: parallel poll configure (board or device)
    //Procedure ibppc(v:Longint);
    /// ibrd: read data bytes (board or device)
    Function Read : String;
    /// ibrda: read data bytes asynchronously (board or device)
    Procedure ReadAsync(Buf:Pointer;NumBytes:Integer);
    /// ibrdf: read data bytes to file (board or device)
    //Procedure ibrdf(file_path:PChar);
    /// ibrpp: perform a parallel poll (board or device)
    //Procedure ibrpp(ppr:PChar);
    /// ibrsc: request system control (board)
    //Procedure ibrsc(v:Longint);
    /// ibrsp: conduct serial poll (device)
    //Procedure ibrsp(spr:PChar);
    /// ibrsv: request service (board)
    //Procedure ibrsv(v:Longint);
    /// ibsad: set secondary GPIB address (board or device)
    //Procedure ibsad(v:Longint);
    /// ibsic: perform interface clear (board)
    //Procedure ibsic:Longint;
    /// ibsre: set remote enable (board)
    //Procedure ibsre(v:Longint);
    /// ibstop: abort asynchronous i/o operation (board or device)
    //Procedure ibstop(ud:Longint);
    /// ibtmo: adjust io timeout (board or device)
    Procedure SetTimeout(v:TGpibTimeout);
    /// ibtrg: trigger device (device)
    //Procedure ibtrg:Longint;
    /// ibwait: wait for event (board or device)
    //Procedure Wait(Mask:Longint);
    /// ibwrt: write data bytes (board or device)
    Procedure Write(St:String);
    /// ibwrta: write data bytes asynchronously (board or device)
    Procedure WriteAsync(St:String);
    /// ibwrtf: write data bytes from file (board or device)
    Procedure WriteFile(FileName:String);
    class Function ErrorString(iberr:Longint):String;
  End;

Const
  ReadBufferIncrement = 10;//24*200;

Implementation
Uses PasGpibUtils,BaseUnix;

{ TGPIB }

Constructor TGPIB.Create(ADeviceName: String);
Begin
  FHandle := Find(ADeviceName);
  if FHandle < 0 then
    raise EGPIB.CreateFmt('Error opening the device: status = $%04X, error = %s; Did you forget to connect the GPIB board, insert the kernel module, or run gpib_config?',[ibsta,ErrorString(iberr)]);
End;

Destructor TGPIB.Destroy;
Begin
  Inherited Destroy;
End;

Function TGPIB.Find(Name: String): Integer;
Begin
  Result := ibfind(@Name[1]);
End;

Function TGPIB.Read: String;
Var Pos    : Integer;
    Status : TIBStaBits;
    F      : File;
Begin
{$IFNDEF ReadWithIBRDF}
  Pos := 1;
  SetLength(Result,ReadBufferIncrement);
  repeat
    { read }
    Status := ibrd(FHandle,Result[Pos],Length(Result)-Pos+1);
    { check status }
    CheckStatus(Status);
    Pos := Pos + ibcnt;
    { if not complete: increase buffer size }
    if Status and ENDS = 0 then
      SetLength(Result,Length(Result)+ReadBufferIncrement);
  Until ((Status and ENDS) <> 0) or SigPending(SIGINT);
  SetLength(Result,Pos-1);
{$ELSE}
  // use ibrdf() instead of ibrd() since the R&S FSEB seems not to like
  // multiple calls to ibrd(), probably this is not FSEB specific 
  Assign(F,'/tmp/gpib-rdf.bin');
  Erase(F);
  if IOResult <> 0 then ;
  { read }
  Status := ibrdf(FHandle,'/tmp/gpib-rdf.bin');//Result[Pos],Length(Result)-Pos+1);
  { check status }
  CheckStatus(Status);
  Assign(F,'/tmp/gpib-rdf.bin');
  Reset(F,1);
  if IOResult <> 0 then Exit;
  Pos := FileSize(F)-1;  // trailing #10
  SetLength(Result,Pos);
  BlockRead(F,Result[1],Pos);
  Close(F);
{$ENDIF}
End;

Procedure TGPIB.ReadAsync(Buf:Pointer;NumBytes:Integer);
Var Status : Integer;
Begin
  Status := ibrda(FHandle,Buf,NumBytes);
End;

Procedure TGPIB.SetTimeout(v: TGpibTimeout);
Begin
  CheckStatus(ibtmo(FHandle,v));
End;

Procedure TGPIB.Write(St: String);
Begin
  CheckStatus(ibwrt(FHandle,St[1],Length(St)));
End;

Procedure TGPIB.WriteAsync(St: String);
Begin
  CheckStatus(ibwrta(FHandle,St[1],Length(St)));
End;

Procedure TGPIB.WriteFile(FileName: String);
Begin
  CheckStatus(ibwrtf(FHandle,@FileName[1]));
End;

class Function TGPIB.ErrorString(iberr: Longint): String;
Begin
  Result := gpib_error_string(iberr);
End;

Procedure TGPIB.CheckStatus(Status:TIBStaBits);
Begin
  Status := Status and DeviceStatusMask;
  if (Status and CMPL = 0) or (Status and (ERR or TIMO) <> 0) then  // TODO: what is with RQS?
    raise EGPIB.CreateFmt('Error: Status = $%04X, Error = %s',[Status,ErrorString(iberr)]);
End;

End.

