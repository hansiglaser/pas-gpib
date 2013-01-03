Program TestKeithley2010;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, LinuxGPIB, OoGPIB, DevComGPIB, DevComRS232, Keithley2010, Serial, Keyboard;

Var Handle : Integer;
    Status : Integer;
    Buffer : Array[0..10] of Char;

    GPIB  : TGPIB;
    DCG   : TGPIBCommunicator;
    DCS   : TRS232Communicator;
    K2010 : TKeithley2010;

Begin


  DCG := TGPIBCommunicator.Create('2010');
  WriteLn(DCG.Query('*IDN?'));
  WriteLn(DCG.Query(':DATA:FRESH?'));

(*  DCS := TRS232Communicator.Create('/dev/ttyUSB0',19200,8,NoneParity,0,[]);
  WriteLn(DCS.Query('*IDN?'));
  WriteLn(DCS.Query(':DATA:FRESH?'));*)

  K2010 := TKeithley2010.Create(DCG);

  WriteLn(K2010.Fetch);
  WriteLn(K2010.FreshData);

  While not KeyPressed do
    Write(^M,K2010.FreshData);
  WriteLn;

  K2010.Free;
  DCS.Free;
  DCG.Free;

Halt;
  GPIB := TGPIB.Create('2010');
  GPIB.SetTimeout(T100ms);
  GPIB.Write('*IDN?');
  WriteLn('+',GPIB.Read,'+');
  WriteLn(GPIB.ErrorString(iberr));
  GPIB.Write(':DATA:FRESH?');
  WriteLn('+',GPIB.Read,'+');
  GPIB.Free;

Halt;
  Handle := ibfind('2010');
  if Handle < 0 then
    Begin
      WriteLn('Error opening the device: status = $',IntToHex(ibsta,4),', error = ',iberr);
      Halt(1);
    End;

  Status := ibpad(Handle, 8);
  if Status <> Integer(CMPL) then
    Begin
      WriteLn('Error: status = $',IntToHex(Status,4),', error = ',iberr);
      Halt(1);
    End;

  // Adjust io timeout
  Status := ibtmo(Handle, Integer(T100ms));
  if Status <> Integer(CMPL) then
    Begin
      WriteLn('Error: status = $',IntToHex(Status,4),', error = ',iberr);
      Halt(1);
    End;

  strcat(Buffer,'*IDN?');
  Status := ibwrt(Handle,Buffer,StrLen(Buffer));
  if Status <> Integer(ENDS) or Integer(CMPL) then
    Begin
      WriteLn('Error: status = $',IntToHex(Status,4),', error = ',iberr);
      Halt(1);
    End;

  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
(*  if Status <> Integer(ENDS) or Integer(CMPL) then
    Begin
      WriteLn('Error: status = $',IntToHex(Status,4),', error = ',iberr);
      Halt(1);
    End;*)

//  WriteLn(Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);
  Status := ibrd(Handle,Buffer,sizeof(Buffer));
WriteLn('Status = ',Status,', cnt = ',ibcnt,', cntl = ',ibcntl,' Buffer = ',Buffer);


End.

