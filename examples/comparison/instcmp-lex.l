(**
 * InstCmp Parser - Lex
 *)

%{

%}

%%

[0-9]+	                Begin yylval.yyinteger := StrToInt(yytext);       return(TOK_INT); End;
[0-9+-][0-9.eE+-]+	Begin yylval.yydouble  := StrToFloat(yytext);     return(TOK_DOUBLE); End;

[0-9_A-Za-z]+           Begin yylval.yyshortstring := yytext; return(TOK_ID); End;

\'(\\.|[^'])*\'         Begin
    //WriteLn('Single-Rule String in line ',yylineno,': +',yytext,'+');
    yylval.yyshortstring := Copy(yytext,2,Length(yytext)-2);
    return(TOK_STRING);
  End;

"#"[^\n]*\n	return(TOK_EOL); (* ignore comments *)
[ \t]		; (* ignore non-newline whitespaces *)
[\r\n]+		return(TOK_EOL);

.               returnc(yytext[1]);

%%

