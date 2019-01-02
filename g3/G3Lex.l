%option noyywrap
/* just like Unix wc */
%{
#include<stdio.h>
#include<string.h>
int chars = 0;
int words = 0;
int lines = 0;
%}
ewline		\n
tab		\t
alfa 		[A-Za-z_]
digit		[0-9]
number		({digit}+)
realnumber	{digit}*"."{digit}+
char		'[^"\n]'{1,4}
string	 	\"[^"]*\"
ident		{alfa}({alfa}|{digit})*
comment		"/*""/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/"
separator	(newline|tab|" ")
invalid 	[ !@#$%&"?""|"~ ]
%%
";"		{ printf(" ; \n");};
"="		{ printf(" = \n");};
"{"		{ printf(" { \n");};
"}"		{ printf(" } \n");};
if		{ printf(" if \n");};
then            { printf(" then \n");};
while		{ printf(" while \n");};
do		{ printf(" do \n");};
{number}	{ printf(" n (%s)\n",yytext);};
{ident}	        { printf(" v (%s)\n",yytext);};
{separator}	|
{comment}	|
{newline}	;		
{invalid}	{ printf(" erro \n");};
%%
int main()
{
yylex();
printf("\n Fim Normal\n");
}
