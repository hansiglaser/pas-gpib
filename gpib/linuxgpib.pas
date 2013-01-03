{**************************************************************************
                          ib.h  -  header file for gpib library
                             -------------------

    copyright            : (C) 2002 by Frank Mori Hess
    email                : fmhess@users.sourceforge.net
    converted by         : glaser@ict.tuwien.ac.at / Johann.Glaser@gmx.at
 ************************************************************************** }
{**************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ************************************************************************** }

Unit LinuxGPIB;

{$MODE ObjFPC}

Interface

{$LINKLIB gpib}

{****************************************************************************
  Automatically converted by H2Pas 1.0.0 from ib.h and gpib_user.h
  from the libgpib0-dev 3.2.11-0.2 Debian package using the commands
    # h2paspp -I -oibnew.h ib.h
    # h2pas -d -e -c -l gpib -o linuxgpib.pas -pr -u LinuxGPIB -v ibnew.h
  and manual changes by Johann Glaser.
*****************************************************************************}

Const
 GPIB_MAX_NUM_BOARDS      = 16;
 GPIB_MAX_NUM_DESCRIPTORS = $1000;

Type TIBStaBitNumbers = Longint;
Const
  DCAS_NUM  =  0;
  DTAS_NUM  =  1;
  LACS_NUM  =  2;
  TACS_NUM  =  3;
  ATN_NUM   =  4;
  CIC_NUM   =  5;
  REM_NUM   =  6;
  LOK_NUM   =  7;
  CMPL_NUM  =  8;
  EVENT_NUM =  9;
  SPOLL_NUM = 10;
  RQS_NUM   = 11;
  SRQI_NUM  = 12;
  END_NUM   = 13;
  TIMO_NUM  = 14;
  ERR_NUM   = 15;

{ IBSTA status bits (returned by all functions)  }
Type TIBStaBits = Longint;
Const
  DCAS  = 1 shl DCAS_NUM;   { device clear state  }
  DTAS  = 1 shl DTAS_NUM;   { device trigger state  }
  LACS  = 1 shl LACS_NUM;   { GPIB interface is addressed as Listener  }
  TACS  = 1 shl TACS_NUM;   { GPIB interface is addressed as Talker  }
  ATN   = 1 shl ATN_NUM;    { Attention is asserted  }
  CIC   = 1 shl CIC_NUM;    { GPIB interface is Controller-in-Charge  }
  REM   = 1 shl REM_NUM;    { remote state  }
  LOK   = 1 shl LOK_NUM;    { lockout state  }
  CMPL  = 1 shl CMPL_NUM;   { I/O is complete }
  EVENT = 1 shl EVENT_NUM;  { DCAS, DTAS, or IFC has occurred  }
  SPOLL = 1 shl SPOLL_NUM;  { board serial polled by busmaster  }
  RQS   = 1 shl RQS_NUM;    { Device requesting service }
  SRQI  = 1 shl SRQI_NUM;   { SRQ is asserted  }
  ENDS  = 1 shl END_NUM;    { EOI or EOS encountered  }
  TIMO  = 1 shl TIMO_NUM;   { Time limit on I/O or wait function exceeded  }
  ERR   = 1 shl ERR_NUM;    { Function call terminated on error  }

  DeviceStatusMask = ERR or TIMO or ENDS or CMPL or RQS;

  BoardStatusMask = ERR or TIMO or ENDS or CMPL or SPOLL or EVENT or LOK or REM or CIC or ATN or TACS or LACS or DTAS or DCAS or SRQI;

{ IBERR error codes }
Type TIBErrCode = Longint;
Const
  EDVR =  0; { system error }
  ECIC =  1; { not CIC }
  ENOL =  2; { no listeners }
  EADR =  3; { CIC and not addressed before I/O }
  EARG =  4; { bad argument to function call }
  ESAC =  5; { not SAC }
  EABO =  6; { I/O operation was aborted }
  ENEB =  7; { non-existent board (GPIB interface offline)  }
  EDMA =  8; { DMA hardware error detected  }
  EOIP = 10; { new I/O attempted with old I/O in progress   }
  ECAP = 11; { no capability for intended opeation  }
  EFSO = 12; { file system operation error  }
  EBUS = 14; { bus error  }
  ESTB = 15; { lost serial poll bytes  }
  ESRQ = 16; { SRQ stuck on  }
  ETAB = 20; { Table Overflow  }

{ Timeout values and meanings  }
Type TGpibTimeout = Longint;
Const
  TNONE  =  0;   { Infinite timeout (disabled)      }
  T10us  =  1;   { Timeout of 10 usec (ideal)       }
  T30us  =  2;   { Timeout of 30 usec (ideal)       }
  T100us =  3;   { Timeout of 100 usec (ideal)      }
  T300us =  4;   { Timeout of 300 usec (ideal)      }
  T1ms   =  5;   { Timeout of 1 msec (ideal)        }
  T3ms   =  6;   { Timeout of 3 msec (ideal)        }
  T10ms  =  7;   { Timeout of 10 msec (ideal)       }
  T30ms  =  8;   { Timeout of 30 msec (ideal)       }
  T100ms =  9;   { Timeout of 100 msec (ideal)      }
  T300ms = 10;   { Timeout of 300 msec (ideal)      }
  T1s    = 11;   { Timeout of 1 sec (ideal)         }
  T3s    = 12;   { Timeout of 3 sec (ideal)         }
  T10s   = 13;   { Timeout of 10 sec (ideal)        }
  T30s   = 14;   { Timeout of 30 sec (ideal)        }
  T100s  = 15;   { Timeout of 100 sec (ideal)       }
  T300s  = 16;   { Timeout of 300 sec (ideal)       }
  T1000s = 17;   { Timeout of 1000 sec (maximum)    }

{ End-of-string (EOS) modes for use with ibeos  }
Type TEOSFlags = Longint;
Const
  EOS_MASK = $1c00;
  REOS     = $0400;   { Terminate reads on EOS }
  XEOS     = $0800;   { assert EOI when EOS char is sent  }
  BIN      = $1000;   { Do 8-bit compare on EOS }

{ GPIB Bus Control Lines bit vector  }
Type TBusControlLine = SmallInt;
Const
  ValidDAV  = $01;
  ValidNDAC = $02;
  ValidNRFD = $04;
  ValidIFC  = $08;
  ValidREN  = $10;
  ValidSRQ  = $20;
  ValidATN  = $40;
  ValidEOI  = $80;
  ValidALL  = $ff;
  BusDAV    = $0100;  { DAV  line status bit  }
  BusNDAC   = $0200;  { NDAC line status bit  }
  BusNRFD   = $0400;  { NRFD line status bit  }
  BusIFC    = $0800;  { IFC  line status bit  }
  BusREN    = $1000;  { REN  line status bit  }
  BusSRQ    = $2000;  { SRQ  line status bit  }
  BusATN    = $4000;  { ATN  line status bit  }
  BusEOI    = $8000;  { EOI  line status bit  }

{ Possible GPIB command messages  }
Type TCmdByte = Byte;
Const
  GTL =  $1;   { go to local			 }
  SDC =  $4;   { selected device clear 	 }
  PPC =  $5;   { parallel poll configure	 }
  GET =  $8;   { group execute trigger 	 }
  TCT =  $9;   { take control 		 }
  LLO = $11;   { local lockout		 }
  DCL = $14;   { device clear 		 }
  PPU = $15;   { parallel poll unconfigure 	 }
  SPE = $18;   { serial poll enable 		 }
  SPD = $19;   { serial poll disable 		 }
  LAD = $20;   { value to be 'ored' in to obtain listen address  }
  UNL = $3F;   { unlisten 			 }
  TAD = $40;   { value to be 'ored' in to obtain talk address    }
  UNT = $5F;   { untalk 			 }
  SAD = $60;   { my secondary address (base)  }
  PPE = $60;   { parallel poll enable (base)	 }
  PPD = $70;   { parallel poll disable	 }

Type TPPEBits = Longint;
Const
  PPC_DISABLE  = $10;
  PPC_SENSE    = $8;      { parallel poll sense bit }
  PPC_DIO_MASK = $7;

Function MLA(Addr:Cardinal) : TCmdByte; inline;
Function MTA(Addr:Cardinal) : TCmdByte; inline;
Function MSA(Addr:Cardinal) : TCmdByte; inline;
Function PPE_byte(dio_line:Cardinal;Sense:Integer) : TCmdByte; inline;
Function gpib_address_equal(pad1:Cardinal;sad1:Integer;pad2:Cardinal;sad2:Integer):Boolean; inline;

{ max address for primary/secondary gpib addresses }
Const GpibAddrMax = 30;

Type TIBAskOption = Longint;
Const
  IbaPAD            = $1;
  IbaSAD            = $2;
  IbaTMO            = $3;
  IbaEOT            = $4;
  IbaPPC            = $5;     { board only  }
  IbaREADDR         = $6;     { device only  }
  IbaAUTOPOLL       = $7;     { board only  }
  IbaCICPROT        = $8;     { board only  }
  IbaIRQ            = $9;     { board only  }
  IbaSC             = $a;     { board only  }
  IbaSRE            = $b;     { board only  }
  IbaEOSrd          = $c;
  IbaEOSwrt         = $d;
  IbaEOScmp         = $e;
  IbaEOSchar        = $f;
  IbaPP2            = $10;
  IbaTIMING         = $11;    { board only  }
  IbaDMA            = $12;    { board only  }
  IbaReadAdjust     = $13;    { board only  }
  IbaWriteAdjust    = $14;
  IbaEventQueue     = $15;    { board only  }
  IbaSPollBit       = $16;    { board only  }
  IbaSendLLO        = $17;    { board only  }
  IbaSPollTime      = $18;    { device only  }
  IbaPPollTime      = $19;    { board only  }
  IbaEndBitIsNormal = $1a;
  IbaUnAddr         = $1b;    { device only  }
  IbaHSCableLength  = $1f;    { board only  }
  IbaIst            = $20;    { board only  }
  IbaRsv            = $21;    { board only  }
  IbaBNA            = $200;   { device only  }
  { linux-gpib extensions }
  Iba7BitEOS        = $1000;  { board only. Returns 1 if board supports 7 bit eos compares }

Type TIBConfigOption = Longint;
Const
  IbcPAD            = $1;
  IbcSAD            = $2;
  IbcTMO            = $3;
  IbcEOT            = $4;
  IbcPPC            = $5;      { board only  }
  IbcREADDR         = $6;      { device only  }
  IbcAUTOPOLL       = $7;      { board only  }
  IbcCICPROT        = $8;      { board only  }
  IbcIRQ            = $9;      { board only  }
  IbcSC             = $a;      { board only  }
  IbcSRE            = $b;      { board only  }
  IbcEOSrd          = $c;
  IbcEOSwrt         = $d;
  IbcEOScmp         = $e;
  IbcEOSchar        = $f;
  IbcPP2            = $10;     { board only  }
  IbcTIMING         = $11;     { board only, use with TT1Delays }
  IbcDMA            = $12;     { board only  }
  IbcReadAdjust     = $13;
  IbcWriteAdjust    = $14;
  IbcEventQueue     = $15;     { board only  }
  IbcSPollBit       = $16;     { board only  }
  IbcSendLLO        = $17;     { board only  }
  IbcSPollTime      = $18;     { device only  }
  IbcPPollTime      = $19;     { board only  }
  IbcEndBitIsNormal = $1a;
  IbcUnAddr         = $1b;     { device only  }
  IbcHSCableLength  = $1f;     { board only  }
  IbcIst            = $20;     { board only  }
  IbcRsv            = $21;     { board only  }
  IbcBNA            = $200;    { device only  }

Type TT1Delays = Longint;  { used with IbcTIMING }
Const
  T1_DELAY_2000ns = 1;
  T1_DELAY_500ns  = 2;
  T1_DELAY_350ns  = 3;

Const RequestServiceBit = $40;

Type TGpibEvents = SmallInt;
Const
  EventNone   = 0;
  EventDevTrg = 1;
  EventDevClr = 2;
  EventIFC    = 3;

Type TAddr4882 = Word;

Const NOADDR = $FFFF;

{ tells RcvRespMsg() to stop on EOI  }
Const STOPend = $100;

Type TSadSpecialAddress = Longint;
Const
  NO_SAD  = 0;
  ALL_SAD = -(1);

Type TSendEotMode = Longint;
Const
  NULLend = 0;
  DABend  = 1;
  NLend   = 2;

(***** Global Variables *****)

Var IBSta  : TIBStaBits; external name 'ibsta';
    IBCnt  : Integer;    external name 'ibcnt';
    IBErr  : TIBErrCode; external name 'iberr';
    IBCntL : LongInt;    external name 'ibcntl';

(***** 'Multidevice' API Functions *****)

Procedure AllSPoll(board_desc:Longint; addressList:array of TAddr4882; resultList:array of smallint);cdecl;external;
Procedure DevClear(board_desc:Longint; address:TAddr4882);cdecl;external;
Procedure DevClearList(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure EnableLocal(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure EnableRemote(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure FindLstn(board_desc:Longint; padList:array of TAddr4882; resultList:array of TAddr4882; maxNumResults:Longint);cdecl;external;
Procedure FindRQS(board_desc:Longint; addressList:array of TAddr4882; var result:smallint);cdecl;external;
Procedure PassControl(board_desc:Longint; address:TAddr4882);cdecl;external;
Procedure PPoll(board_desc:Longint; var result:smallint);cdecl;external;
Procedure PPollConfig(board_desc:Longint; address:TAddr4882; dataLine:Longint; lineSense:Longint);cdecl;external;
Procedure PPollUnconfig(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure RcvRespMsg(board_desc:Longint; var buffer:pointer; count:Longint; termination:Longint);cdecl;external;
Procedure ReadStatusByte(board_desc:Longint; address:TAddr4882; var result:smallint);cdecl;external;
Procedure Receive(board_desc:Longint; address:TAddr4882; var buffer:pointer; count:Longint; termination:Longint);cdecl;external;
Procedure ReceiveSetup(board_desc:Longint; address:TAddr4882);cdecl;external;
Procedure ResetSys(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure Send(board_desc:Longint; address:TAddr4882; var buffer:pointer; count:Longint; eot_mode:TSendEotMode);cdecl;external;
Procedure SendCmds(board_desc:Longint; var cmds:pointer; count:Longint);cdecl;external;
Procedure SendDataBytes(board_desc:Longint; var buffer:pointer; count:Longint; eotmode:TSendEotMode);cdecl;external;
Procedure SendIFC(board_desc:Longint);cdecl;external;
Procedure SendLLO(board_desc:Longint);cdecl;external;
Procedure SendList(board_desc:Longint; addressList:array of TAddr4882; var buffer:pointer; count:Longint; eotmode:TSendEotMode);cdecl;external;
Procedure SendSetup(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure SetRWLS(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure TestSRQ(board_desc:Longint; var result:smallint);cdecl;external;
Procedure TestSys(board_desc:Longint; addressList:array of TAddr4882; resultList:array of smallint);cdecl;external;
Function ThreadIbsta:Longint;cdecl;external;
Function ThreadIberr:Longint;cdecl;external;
Function ThreadIbcnt:Longint;cdecl;external;
Function ThreadIbcntl:Longint;cdecl;external;
Procedure Trigger(board_desc:Longint; address:TAddr4882);cdecl;external;
Procedure TriggerList(board_desc:Longint; addressList:array of TAddr4882);cdecl;external;
Procedure WaitSRQ(board_desc:Longint; var result:smallint);cdecl;external;

(***** 'Traditional' API Functions *****)

Function ibask(ud:Longint; option:TIBAskOption; var value:Longint):TIBStaBits;cdecl;external;
Function ibbna(ud:Longint; board_name:PChar):TIBStaBits;cdecl;external;
Function ibcac(ud:Longint; synchronous:Longint):TIBStaBits;cdecl;external;
Function ibclr(ud:Longint):TIBStaBits;cdecl;external;
Function ibcmd(ud:Longint; cmd:Array of TCmdByte;cnt:Longint):TIBStaBits;cdecl;external;
Function ibcmda(ud:Longint; cmd:Array of TCmdByte; cnt:Longint):TIBStaBits;cdecl;external;
Function ibconfig(ud:Longint; option:TIBConfigOption; value:Longint) : TIBStaBits;cdecl;external;
Function ibdev(board_index:Longint; pad:Longint; sad:Longint; timo:Longint; send_eoi:Longint;eosmode:TEOSFlags) : TIBStaBits;cdecl;external;
Function ibdma(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibeot(ud:Longint; v:LongBool) : TIBStaBits;cdecl;external;
Function ibeos(ud:Longint; v:TEOSFlags) : TIBStaBits;cdecl;external;
Function ibevent(ud:Longint; var event:TGpibEvents) : TIBStaBits;cdecl;external;
Function ibfind(dev:PChar) : TIBStaBits;cdecl;external;
Function ibgts(ud:Longint; shadow_handshake:Longint) : TIBStaBits;cdecl;external;
Function ibist(ud:Longint; ist:Longint) : TIBStaBits;cdecl;external;
Function iblines(ud:Longint; var line_status:TBusControlLine) : TIBStaBits;cdecl;external;
Function ibln(ud:Longint; pad:Longint; sad:Longint; var found_listener:smallint) : TIBStaBits;cdecl;external;
Function ibloc(ud:Longint) : TIBStaBits;cdecl;external;
Function ibonl(ud:Longint; onl:Longint) : TIBStaBits;cdecl;external;
Function ibpad(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibpct(ud:Longint) : TIBStaBits;cdecl;external;
Function ibppc(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibrd(ud:Longint; var buf; count:Longint) : TIBStaBits;cdecl;external;
Function ibrda(ud:Longint; var buf:pointer; count:Longint) : TIBStaBits;cdecl;external;
Function ibrdf(ud:Longint; file_path:PChar) : TIBStaBits;cdecl;external;
Function ibrpp(ud:Longint; ppr:PChar) : TIBStaBits;cdecl;external;
Function ibrsc(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibrsp(ud:Longint; spr:PChar) : TIBStaBits;cdecl;external;
Function ibrsv(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibsad(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibsic(ud:Longint) : TIBStaBits;cdecl;external;
Function ibsre(ud:Longint; v:Longint) : TIBStaBits;cdecl;external;
Function ibstop(ud:Longint) : TIBStaBits;cdecl;external;
Function ibtmo(ud:Longint; v:TGPIBTimeout) : TIBStaBits;cdecl;external;
Function ibtrg(ud:Longint) : TIBStaBits;cdecl;external;
Function ibwait(ud:Longint; mask:Longint) : TIBStaBits;cdecl;external;
Function ibwrt(ud:Longint; var buf; count:Longint) : TIBStaBits;cdecl;external;
Function ibwrta(ud:Longint; var buf; count:Longint) : TIBStaBits;cdecl;external;
Function ibwrtf(ud:Longint; file_path:PChar) : TIBStaBits;cdecl;external;
Function gpib_error_string(IBErr:TIBErrCode):Pchar;cdecl;external;

Function MakeAddr(PAD:Cardinal;SAD:Cardinal) : TAddr4882; inline;
Function GetPAD(Address:TAddr4882):Cardinal; inline;
Function GetSAD(Address:TAddr4882):Cardinal; inline;

Implementation

Function MLA(Addr:Cardinal) : TCmdByte; inline;
Begin
  Result := Addr or LAD;
End;

Function MTA(Addr:Cardinal) : TCmdByte; inline;
Begin
  Result := Addr or TAD;
End;

Function MSA(Addr:Cardinal) : TCmdByte; inline;
Begin
  Result := Addr or SAD;
End;

Function PPE_byte(dio_line:Cardinal;Sense:Integer) : TCmdByte; inline;
Begin
  Result := PPE or (dio_line - 1) and $7;
  if Sense <> 0 then
    Result := Result or PPC_SENSE;
End;

Function gpib_address_equal(pad1:Cardinal;sad1:Integer;pad2:Cardinal;sad2:Integer):Boolean; inline;
Begin
  if pad1 = pad2 then
    Begin
      if sad1 = sad2 then Exit(true);
      if (sad1 < 0) and (sad2 < 0) then Exit(true);
    End;
  Result := false;
End;

Function MakeAddr(PAD:Cardinal;SAD:Cardinal) : TAddr4882; inline;
Begin
  Result := (PAD and $FF) or ((SAD and $FF) shl 8);
End;

Function GetPAD(Address:TAddr4882):Cardinal; inline;
Begin
  Result := Address and $FF;
End;

Function GetSAD(Address:TAddr4882):Cardinal; inline;
Begin
  Result := (Address shr 8) and $FF;
End;

End.

