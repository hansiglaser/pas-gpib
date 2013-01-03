Unit DevComGPIB;

{$mode objfpc}{$H+}

// use interfaces without IUnknown as ancestor, so they don' have reference
// counting, the classes don't need to be derived from TInterfacedObject, and
// there are no strange problems when destroying objects using our classes
{$interfaces corba}

Interface

Uses
  Classes, SysUtils,
  DevCom, OoGPIB;

Type

  { TGPIBCommunicator }

  TGPIBCommunicator = class(IDeviceCommunicator)
  private
    FGPIB : TGPIB;
  public
    Constructor Create(ADeviceName:String);  // device { name = "..." } from /etc/gpib.conf
    Destructor  Destroy; override;
    Procedure Send(St:String);
    Function  Receive : String;
    Function  Query(St:String):String;
    Procedure SetTimeout(ATimeout:LongInt);   // in us
  End;

Implementation
Uses LinuxGPIB;

{ TGPIBCommunicator }

Constructor TGPIBCommunicator.Create(ADeviceName: String);
Begin
  FGPIB := TGPIB.Create(ADeviceName);
  SetTimeout(100000); // 100ms
End;

Destructor TGPIBCommunicator.Destroy;
Begin
  FGPIB.Free;
  Inherited Destroy;
End;

Procedure TGPIBCommunicator.Send(St: String);
Begin
  FGPIB.Write(St);
End;

Function TGPIBCommunicator.Receive: String;
Begin
  Result := FGPIB.Read;
End;

Function TGPIBCommunicator.Query(St: String): String;
Begin
  Send(St);
  Result := Trim(Receive);
End;

Procedure TGPIBCommunicator.SetTimeout(ATimeout: LongInt);
Var T : TGpibTimeout;
Begin
       if ATimeout <         10 then T := T10us    { Timeout of 10 usec (ideal)       }
  else if ATimeout <         30 then T := T30us    { Timeout of 30 usec (ideal)       }
  else if ATimeout <        100 then T := T100us   { Timeout of 100 usec (ideal)      }
  else if ATimeout <        300 then T := T300us   { Timeout of 300 usec (ideal)      }
  else if ATimeout <       1000 then T := T1ms     { Timeout of 1 msec (ideal)        }
  else if ATimeout <       3000 then T := T3ms     { Timeout of 3 msec (ideal)        }
  else if ATimeout <      10000 then T := T10ms    { Timeout of 10 msec (ideal)       }
  else if ATimeout <      30000 then T := T30ms    { Timeout of 30 msec (ideal)       }
  else if ATimeout <     100000 then T := T100ms   { Timeout of 100 msec (ideal)      }
  else if ATimeout <     300000 then T := T300ms   { Timeout of 300 msec (ideal)      }
  else if ATimeout <    1000000 then T := T1s      { Timeout of 1 sec (ideal)         }
  else if ATimeout <    3000000 then T := T3s      { Timeout of 3 sec (ideal)         }
  else if ATimeout <   10000000 then T := T10s     { Timeout of 10 sec (ideal)        }
  else if ATimeout <   30000000 then T := T30s     { Timeout of 30 sec (ideal)        }
  else if ATimeout <  100000000 then T := T100s    { Timeout of 100 sec (ideal)       }
  else if ATimeout <  300000000 then T := T300s    { Timeout of 300 sec (ideal)       }
  else if ATimeout < 1000000000 then T := T1000s   { Timeout of 1000 sec (maximum)    }
  else                               T := TNONE;   { Infinite timeout (disabled)      }
  FGPIB.SetTimeout(T);
End;

End.

