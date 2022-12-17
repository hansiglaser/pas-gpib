

all: instcmp-parser.pas instcmp-lex.pas

instcmp-parser.pas: instcmp-parser.y
	pyacc instcmp-parser.y
	# as of 2022-11-06, this gives an error
	#   FATAL: cannot open file /usr/lib/fpc/lexyacc/yyparse.cod
	# which I worked around with the following command as root
	#   ln -s /usr/lib/x86_64-linux-gnu/fpc/3.2.2/lexyacc /

instcmp-lex.pas: instcmp-lex.l instcmp-parser.pas
	plex instcmp-lex.l


