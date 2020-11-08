Unit SerialParseI2C;

{$mode objfpc}{$H+}

Interface

Procedure ParseI2C(Filename:String{;Var AModules : TModuleList});

Implementation

Uses SysUtils, yacclib, lexlib;

{ $ DEFINE yydebug}
{$INCLUDE serial-i2c-parser.pas}
{$INCLUDE serial-i2c-lex.pas}

Function MyYyWrap() : Boolean;
Begin
  Close(yyinput);
  Result := true;
End;

Procedure ParseI2C(Filename:String{;Var AModules : TModuleList});
Begin
  Assign(yyinput,Filename);
  {$I-} Reset(yyinput); {$I+}
  if IOResult <> 0 then
    Begin
      WriteLn('Error: File ',Filename,' not found.');
      Exit;
    End;

(*  if not assigned(AModules) then
    AModules := TModuleList.Create;
  ModuleList := AModules;*)

  yylineno := 0;
  yyclear;
  yywrap:= @MyYyWrap;
  try
    yyparse();
  except
    on E : Exception do
      Begin
        yyerror('Exception: ' + E.Message);
        raise;
      End;
  End;
//  yylex_destroy();
End;

End.

