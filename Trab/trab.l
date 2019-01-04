%{
#include "trabalho.tab.h"
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

IF 			"if"
THEN		"then"
ELSE		"else"
WHILE		"while"
INT 		"int"
FAZER			"do"


IDENT		{ALFA}({ALFA}|{DIGIT})*
WS              [ \t]*
ATRIB			"="
LP              "("
RP              ")"
PTV             ";"
VIRGULA			","
FCHAVE			"}"
ACHAVE			"{"
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

{FAZER} 	{
		return _DO;
		}
{ACHAVE} 	{
		return _ABRECHAVE;
		}
{FCHAVE} 	{
		return _FECHACHAVE;
		}
{VIRGULA} 	{
		return _VIRG;
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
{NUMBER}        {
		strcpy(yylval.t.symbol, yytext);
                return _n;
                }
{TINT}          {
   return_TINT;
   }
{PRINT}         {
		return _PRINT;
		}
{IF}		{
		return _IF;
		}
{THEN}		{
		return _THEN;
		}
{ELSE}		{
		return _ELSE;
		}
{WHILE}		{
		return _WHILE;
		}
{INT}		{
		strcpy(yylval.t.symbol, yytext); 
		return _INT;
		}
{IDENT}         {
		strcpy(yylval.t.symbol, yytext); 
		return _id;
		}
{RET}           {
                return yytext[0];
                }
