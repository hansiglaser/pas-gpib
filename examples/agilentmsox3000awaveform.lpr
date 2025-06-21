(**
 * Download a waveform from Agilent InfiniiVision MSO-X 3000A oscilloscopes
 *
 * This program just downloads the currently shown waveforms and saves it to a
 * CSV file. No settings on the scope are changed.
 *
 * Warning: This demo does no sanity checking.
 *)
Program AgilentMSOX3000AWaveform;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, Math, PasGpibUtils, DevCom,
  DevComVisa,
  Keithley2600,
  AgilentMSOX3000A;

Var
  Visa     : String;
  Source   : TWaveformSource;
  Filename : String;
  I        : Integer;
  Comm     : IDeviceCommunicator;
  CommObj  : TObject;
  MSOX     : TAgilentMSOX3000A;
  Waveform : TWaveform;

Procedure Usage;
Begin
  WriteLn('Usage: ',ParamStr(0),': visa source [filename]');
  WriteLn;
  WriteLn('  visa      instrument VISA string, e.g., ''USB0::0x0957::0x17a6::*::0::INSTR''');
  WriteLn('            or ''TCPIP::a-mx3024a-91958.home::5025::SOCKET''');
  WriteLn('  source    waveform source (channel), one of ``',JoinStr(''', ''', CWaveformSource), '''');
  WriteLn('  filename  (optional) filename to save the waveform data to');
  Halt(1);
End;

Begin
  if (ParamCount < 2) or (ParamCount > 3) then
    Usage;
  Visa     := ParamStr(1);
  I        := Find(ParamStr(2), CWaveformSource);
  if I < 0 then
    Begin
      WriteLn('Invalid waveform source ''',ParamStr(2),'''.');
      Usage;
    End;
  Source   := TWaveformSource(I);
  if ParamCount = 3 then
    Filename := ParamStr(3)
  else
    Filename := 'MSOX-waveform-'+ParamStr(2)+'-'+FormatDateTime('yyyymmdd-hhnnss',Now)+'.csv';

  WriteLn('Contacting Agilent InfiniiVision MSO-X ', Visa);

  Comm := DevComOpen(Visa, CommObj);
  Comm.SetTimeout(10000000);

  { remote instrument }
  MSOX := TAgilentMSOX3000A.Create(Comm);

  MSOX.SetWaveformSource(Source);
  MSOX.SetWaveformSubSource(wsSub0);     // all waveforms have Sub0, only serial SPI and UART have in addition Sub1
  MSOX.SetWaveformFormat(wfByte);
  MSOX.SetWaveformPointsMode(wpmRaw);
  I := MSOX.GetAcquirePoints;
  MSOX.SetWaveformPointsCount(I);    // only gives maximum if MATH is disabled
  I := MSOX.GetWaveformPointsCount;
  WriteLn('Download waveform ',ParamStr(2),' with ',I,' points from ',MSOX.Identify,' to ',Filename);
  Waveform := MSOX.GetWaveformPreamble;
  Waveform.PrintPreamble;
  MSOX.GetWaveformData(Waveform);
  Waveform.ConvToReal;
  Waveform.ConvTimes;
  Waveform.PrintAsciiArt(80, 25, 500e-6, 0.5);
  Waveform.SaveCSV(Filename);

  MSOX.Free;
  CommObj.Free;
End.

