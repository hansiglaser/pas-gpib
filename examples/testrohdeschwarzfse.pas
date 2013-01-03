Program TestRohdeSchwarzFSE;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, DevComGPIB, RohdeSchwarzFSEB;

Var DC    : TGPIBCommunicator;
    FSEB  : TRohdeSchwarzFSEB;
    St    : String;
    F     : Text;

Begin
  { device communicator }
  DC := TGPIBCommunicator.Create('FSEB');
  DC.SetTimeout(5000000{us});
  { remote instrument }
  FSEB := TRohdeSchwarzFSEB.Create(DC);
  WriteLn(FSEB.Date);
  St := FSEB.Screen(ifWmf,'Astra');
  Assign(F,'hcopy.wmf');
  Rewrite(F);
  Write(F,St);
  Close(F);
  WriteLn(FSEB.Date);
End.

