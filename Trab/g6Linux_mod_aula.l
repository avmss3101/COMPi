%{
#include "g6Linux.tab.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
char *yytext;
YYSTYPE yylval;
%}

ALFA 		[A-Za-z_]
PLUS            [\+]
MINUS           [\-]
TIMES           [\*]
DIVIDE          [/]
DIGIT           [0-9]
NUMBER          ({DIGIT}+)
PRINT           "print"
IDENT		{ALFA}({ALFA}|{DIGIT})*
WS              [ \t]*
ATRIB		"="
LP              "("
RP              ")"
PTV             ";"
RET             [\n]

%%

{WS}            {
                /* eat up white space */
                }
{PLUS}          {
                return _MAIS;
                }
{TIMES}         {
                return _MULT;
                }
{MINUS}         {
                return _MENOS;
                }
{DIVIDE}        {
                return _DIVID;
                }
{ATRIB}		{
		return _ATRIB;
		}
{LP}            {
                return _ABREPAR;
                }
{RP}            {
                return _FECHAPAR;
                }
{PTV}		{
		return _PTVIRG;
		}
{TINT}          {
                return_TINT;
                }

{PRINT}         {
		return _PRINT;
		}
{NUMBER}        {
		strcpy(yylval.t.symbol, yytext);
                return _N;
                }
{IDENT}         {
		strcpy(yylval.t.symbol, yytext);
		return _V;
		}
{RET}           {
                return yytext[0];
                }
