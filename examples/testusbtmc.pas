(**
 * Test USB-TMC support
 *
 * This test program scans the USB bus for USB TMC capable devices and prints a
 * list of them. For each device the communication class defined in the unit
 * UsbTmc is used to query some information and to communicate. Secondly,
 * the IDeviceCommunicator for USB TMC is used to communicate.
 *)
Program TestUsbTmc;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,SysUtils,
  LibUsbOop,
  DevCom,{DevComGpib,}DevComRS232,DevComTCP,DevComTCPLeCroy,DevComUSBTMC, UsbTmc;

Var Context : TLibUsbContext;
    Devices : TUSBTMCIntfInfos;
    I       : Integer;
    Strings : TUSBTMCIntfInfoStrings;
    DC      : TUSBTMCUSB488;
    Status  : Byte;
    Dev     : TUSBTMCCommunicator;
Begin
  Context := TLibUsbContext.Create;
  // scan the USB bus for all USB TMC capable devices
  Devices := TUSBTMCUSB488.Scan(Context);
  if Length(Devices) = 0 then
    Begin
      WriteLn('No USB devices with TMC and GPIB found');
      Halt;
    End;
  WriteLn('Found ',Length(Devices),' USB TMC devices.');
  For I := 0 to Length(Devices)-1 do
    With Devices[I] do
      Begin
        // print some info available via standard USB descriptors
        WriteLn('Device ',I+1,' at $',IntToHex(PtrUInt(Device),2*SizeOf(PtrUInt)));
        Strings := TUSBTMCUSB488.GetInfoStrings(Context,Devices[I]);
        WriteLn('  Vendor         = $',IntToHex(DevDescr.idVendor, 4),' ',Strings.sVendor);
        WriteLn('  Product        = $',IntToHex(DevDescr.idProduct,4),' ',Strings.sProduct);
        WriteLn('  Serial         = ',Strings.sSerial       );
        WriteLn('  bConfiguration = ',ConfigDescr.bConfigurationValue,' ',Strings.sConfiguration);
        WriteLn('  bInterface     = ',IntfDescr.bInterfaceNumber,     ' ',Strings.sInterface    );
        //
        try
          try
            // test stuff from unit UsbTmc
            DC := TUSBTMCUSB488.Create(Context,Devices[I]);
            WriteLn('  Capabilities: BCDVersion = ',      IntToHex(DC.UsbTmcBcdVersion,4),
                                  ', IndicatorPulse = ',  DC.CapIndicatorPulse,
                                  ', TalkOnly = ',        DC.CapTalkOnly,
                                  ', ListenOnly = ',      DC.CapListenOnly,
                                  ', TermChar = ',        DC.CapTermChar,
                                  ', Usb488BcdVersion = ',IntToHex(DC.Usb488BcdVersion,4),
                                  ', USB488.2 = ',        DC.CapUSB4882,
                                  ', RenControl = ',      DC.CapRenControl,
                                  ', Trigger = ',         DC.CapTrigger,
                                  ', Scpi = ',            DC.CapScpi,
                                  ', SR1 = ',             DC.CapSR1,
                                  ', RL1 = ',             DC.CapRL1,
                                  ', DT1 = ',             DC.CapDT1);
            Status := DC.IndicatorPulse;
            WriteLn('  IndicatorPulse: Status = ',Status);
            // most direct communication
            DC.Send('*IDN?'^J);
            WriteLn('  Identity = ''',DC.Recv(100),'''');
            DC.Send('*IDN?'^J);
            WriteLn('  Identity = ''',DC.Recv(100),'''');
            // next layer: use device communicator (without an instrument)
            Dev := TUSBTMCCommunicator.Create(DC);
            Sleep(100);
            WriteLn('  Identity = ''',Dev.Query('*IDN?'),'''');
          finally
            Dev.Free;
            DC.Free;
          End;
        except
          on E : Exception do
            WriteLn(E.Message);
        End;
      End;
End.

