Program ComparisonMgr;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, fpvectorialpkg
  { you can add units after this };

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Title := 'Comparison Manager';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
End.

