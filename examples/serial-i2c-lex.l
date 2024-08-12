(*
 * Lexer to parse the output format of serialparse for I2C serial decode bus
 *
 * Input format:
 *  - each I2C event is in a single line.
 *  - each line is composed of <timestamp>,<event>
 *  - the <timestamp> is the time relative to the trigger and given in
 *    microseconds
 *  - the <event> is e.g., "INIT IN", "A8Ra", ...
 *
 * See the header in serial-i2c-parser.y for the explanation of tokens.
 *)

%{

/// parse the timestamp of an I2C event
Procedure ParseTimestamp(St:String;Out I2CEvent:TI2CEvent);
Var I : Integer;
Begin
  I := Pos(',',St);
  if I <= 0 then
    raise Exception.Create('Invalid line '''+St+'''');
  SetLength(St,I-1);
  I2CEvent.Timestamp := StrToFloat(St);
End;

/// parse an I2C event with timestamp and data
Procedure ParseAll(St:String;Out I2CEvent:TI2CEvent);
Var I : Integer;
Begin
  ParseTimestamp(St, I2CEvent);
  I := Pos(',',St);
  if I <= 0 then
    raise Exception.Create('Invalid line '''+St+'''');
  I2CEvent.Data := StrToInt('$'+ Copy(St, I+1, 2));
End;

%}

%%

[0-9.eE+-]+,INIT\ IN\n               Begin DbgWr('TOK_INIT_IN     : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_INIT_IN);      End;
[0-9.eE+-]+,INIT\ OUT\n              Begin DbgWr('TOK_INIT_OUT    : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_INIT_OUT);     End;
[0-9.eE+-]+,FINAL\ IN\n              Begin DbgWr('TOK_FINAL_IN    : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_FINAL_IN);     End;
[0-9.eE+-]+,FINAL\ OUT\n             Begin DbgWr('TOK_FINAL_OUT   : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_FINAL_OUT);    End;
[0-9.eE+-]+,UNKNOWN\n                Begin DbgWr('TOK_UNKNOWN     : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_UNKNOWN);      End;
[0-9.eE+-]+,ALIASED\n                Begin DbgWr('TOK_ALIASED     : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_ALIASED);      End;
[0-9.eE+-]+,START\n                  Begin DbgWr('TOK_START       : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_START);        End;
[0-9.eE+-]+,STOP\n                   Begin DbgWr('TOK_STOP        : '+yytext); ParseTimestamp(yytext, yylval.yyTI2CEvent); return(TOK_STOP);         End;
[0-9.eE+-]+,[0-9A-F][0-9A-F]Wa\n     Begin DbgWr('TOK_ADDR_WR_ACK : '+yytext); ParseAll      (yytext, yylval.yyTI2CEvent); return(TOK_ADDR_WR_ACK);  End;
[0-9.eE+-]+,[0-9A-F][0-9A-F]W~a\n    Begin DbgWr('TOK_ADDR_WR_NACK: '+yytext); ParseAll      (yytext, yylval.yyTI2CEvent); return(TOK_ADDR_WR_NACK); End;
[0-9.eE+-]+,[0-9A-F][0-9A-F]Ra\n     Begin DbgWr('TOK_ADDR_RD_ACK : '+yytext); ParseAll      (yytext, yylval.yyTI2CEvent); return(TOK_ADDR_RD_ACK);  End;
[0-9.eE+-]+,[0-9A-F][0-9A-F]R~a\n    Begin DbgWr('TOK_ADDR_RD_NACK: '+yytext); ParseAll      (yytext, yylval.yyTI2CEvent); return(TOK_ADDR_RD_NACK); End;
[0-9.eE+-]+,[0-9A-F][0-9A-F]a\n      Begin DbgWr('TOK_DATA_ACK    : '+yytext); ParseAll      (yytext, yylval.yyTI2CEvent); return(TOK_DATA_ACK);     End;
[0-9.eE+-]+,[0-9A-F][0-9A-F]~a\n     Begin DbgWr('TOK_DATA_NACK   : '+yytext); ParseAll      (yytext, yylval.yyTI2CEvent); return(TOK_DATA_NACK);    End;

%{
// the next two are just to catch invalid data
%}
.*             Begin  WriteLn('Invalid '''+yytext+''''); return(TOK_FINAL_OUT); End;
.              Begin  WriteLn('Invalid '''+yytext+''''); returnc(yytext[1]);    End;


