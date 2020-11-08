(**
 * Parse and interpret a serial decode bus file
 *
 * This program reads a serial decode bus file, which was recorded with e.g.,
 * agilentmsox3000a-serial. It interprets the content and prints it to the
 * screen.
 *
 * Currently only the I2C protocol is supported.
 *
 * Warning: The parser is very picky regarding protocol errors and ambiguous
 * sequences.
 *
 * The I2C protocol interpreter uses the parser generator "Yacc", more precisely
 * pyacc for Pascal code. This limits it to unambituous grammar and has
 * dificulties with certain cases like restarting I2C transfer with and without
 * a stop condition.
 *
 * It would be easier to implement a manual FSM which handles each event as
 * a state transitions.
 *
 *)
Program SerialParse;

Uses SysUtils, SerialParseI2C;

Type TParseProtocol = (ppI2C);

Var I        : Integer;
    Filename : String;
    Protocol : TParseProtocol;

Procedure Usage(ExitCode:Integer);
Begin
  Write('Usage: ',ParamStr(0),' [-h|--help] [-i] infile');
  WriteLn;
  WriteLn('  -h|--help  Prints this help');
  WriteLn('  -i         selects I2C decoding (default)');
  WriteLn('  infile     Input file');
  if ExitCode >= 0 then
    Halt(ExitCode);
End;

Begin
  // prepare data and parse parameters
  Protocol := ppI2C;
  Filename := '';
  if ParamCount < 1 then Usage(1);
  I := 1;
  while I < ParamCount do
    Begin
      Case ParamStr(I) of
        '-h',
        '--help' : Usage(0);
        '-i'     : Protocol := ppI2C;
      else
        Usage(1);
      End;
      Inc(I);
    End;
  Filename := ParamStr(I);

  // parse
  Case Protocol of
    ppI2C : ParseI2C(Filename);
  else
    WriteLn('Unsupported protocol');
  End;
End.

