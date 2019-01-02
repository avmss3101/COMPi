%{
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
extern int yylex ();
 
#define NADA		9999
#define FRACASSO	9998
#define ACHOUDIFVAR	9997

char *msg4 = "unknow entity in source program";

typedef enum {
	Variable,
	Constant,
	Temporary,
	Function,
	Procedure
} Entity;

/*
SymbTab: 1as 50 entradas p/ simbolos do fonte
e �ltimas p/ as temporarias
*/
typedef struct {
  char     asciiOfSource [20];
  Entity   entt;
  int      value;
} SymbTab;

SymbTab symbTab [100];
	

int	topTab=0;   // first 50 entries are programmer symbols
int	topTemp=50; // last  50 entries are temporary

int searchSymbTab (char *symb){ 
  int k;
  for (k = 0; k < topTab; k++)
    if (strcmp(symb,symbTab[k].asciiOfSource) == 0)
      return k;
  return topTab;
};

int insertSymbTab (char *symb, Entity whichEntt) {
  int existingSym, current, aux;
  
  existingSym = searchSymbTab (symb);
  if (existingSym < topTab) return existingSym;
  current = topTab;
  if ((whichEntt == Variable) || (whichEntt == Constant)) {
     strcpy(symbTab[current].asciiOfSource,symb);
     symbTab[current].entt = whichEntt;
     }
  else {
    char * ptMsg = (char *) malloc (80);
    strcpy(ptMsg,"Unknown entity type: "); 
    strcat(ptMsg,symb); 
    yyerror (ptMsg);
    };
  if (whichEntt == Constant)
     symbTab[current].value = atoi(symb);
  if (whichEntt == Variable) 
     symbTab[current].value = 0;  
  topTab++;
  return current;
};
int temp () { 
	char nomeTemporaria[4];
	int retorno;
        sprintf(nomeTemporaria,"t%d",topTemp-50);
	strcpy(symbTab[topTemp].asciiOfSource,nomeTemporaria);
	symbTab[topTemp].entt = Temporary;
        retorno=topTemp;
	topTemp++;
	return (retorno);
};
void printSymbTable () {
int i, j, inicio, fimTrecho;
inicio=0;
j=0;
fimTrecho = topTab-1;// trecho dos s�mbolos do programa  
while (j <= 1) {
  for (i=inicio; i <= fimTrecho; i++) { 
    switch (symbTab[i].entt) {
      case Variable: printf("> Variable: ");break;
      case Constant: printf("> Numerical Constant: ");break;
      case Temporary: printf("> Temporary: ");break;
      case Function: printf("> Function: ");break;
      case Procedure: printf("> Procedure: ");break;
      default: yyerror(msg4);break;
    };
    printf("%s ", symbTab[i].asciiOfSource);
    printf("%d \n", symbTab[i].value);
    };// do for
  j++;
  inicio = 50;
  fimTrecho=topTemp-1;  // trecho das tempor�rias
}; // do while
}; // da function printSymbTable

typedef enum {
ADD,
SUB,
MULT,
STO,
PRINT,
J,
JF
} Operador;

struct Quadrupla {
	Operador        op;
	int             operando1;
	int             operando2;
	int             operando3;
	} quadrupla [ 100 ];

int prox;

void gera (Operador codop,int end1,int end2,int end3)
{
	quadrupla [prox].op = codop;
	quadrupla [prox].operando1 = end1;
	quadrupla [prox].operando2 = end2;
	quadrupla [prox].operando3 = end3;
	prox++;
	};


void remenda (int posM, Operador codop,int end1,int end2,int end3)
{
	quadrupla [posM].op = codop;
	quadrupla [posM].operando1 = end1;
	quadrupla [posM].operando2 = end2;
	quadrupla [posM].operando3 = end3;
	};


void imprimeQuadrupla(){
  int r; 
  for(r=0;r<prox;r++) 
    printf("%d %d %d %d\n",
            quadrupla[r].op,                
               quadrupla[r].operando1,
                  quadrupla[r].operando2,
                     quadrupla[r].operando3);
  
}; //da funcao imprimeQuadrupla

void finaliza () {
  printSymbTable ();
  imprimeQuadrupla ();
  printf("End normal compilation! \n");
  exit(0);
  };

void yyerror(const char *str)
{
  printf("error: %s\n",str);
  exit (1);
};

int yywrap()
{
  return 1;
};

int main()
{
  printf("\n \n>G6 \n>"); 
  yyparse();
  return 0;
};
%}
%union{
  struct T{
    char symbol[21];
    int intval;}t;
 }
%token _ATRIB _EOF _ABREPAR _ABRECHAVE _FECHACHAVE _FECHAPAR  _VIRG _PTVIRG
%token _MAIS _MENOS _MULT _PRINT _IF _THEN _ELSE _WHILE _INT _DO
%token _ERRO
%token _n _id //MESMA COISA QUE V
%type<t> E T F _n _id _INT M N // t do symbol
%%
/* 
regras da gramatica e acoes semanticas
*/

P	:	D _ABRECHAVE C _FECHACHAVE 
		{ 
		finaliza ();  
		}
    ;

D	:	D V _PTVIRG // sem acao semantica
	|	V _PTVIRG 
	;

V	:	V _VIRG _id  
		{ insertSymbTab($3.symbol, Variable);}
	|	_INT _id {insertSymbTab($2.symbol, Variable); }
	;

B	:	_ABRECHAVE C _FECHACHAVE //ok
	|	S
	; // ok

C	:	C _PTVIRG S
	|	S 
	; //ok

S	:	_IF _ABREPAR E _FECHAPAR M _THEN B M _ELSE B 
		{ remenda($5.intval,JF,$3.intval,$8.intval+1,NADA); //intQuadr eh intval
		remenda($8.intval,J,prox,NADA,NADA);
		printf("\n");}

	|	_WHILE N _ABREPAR E _FECHAPAR M _DO B { gera(J, $2.intval,NADA,NADA); 
		remenda($6.intval,JF,$4.intval,prox,NADA);
		printf("\n");}

	|	_id _ATRIB E //ok
		{
		$1.intval = searchSymbTab($1.symbol); //o so tem um paramentro
		gera(STO,$3.intval,$1.intval,NADA);
		printf("\n");}
	|	_PRINT _ABREPAR E _FECHAPAR { gera(PRINT,$3.intval, NADA, NADA); 
		printf("\n");}
	;

E	:	E _MAIS T{$$.intval = temp(); 
			gera(ADD, $1.intval, $3.intval,$$.intval);} //ok
	|	E _MENOS T{$$.intval = temp(); 
			gera(SUB, $1.intval, $3.intval,$$.intval);} //ok
	|	T{$$.intval = $1.intval;}
	; //ok

T	:	T _MULT F {$$.intval = temp();     //ok
			gera(MULT, $1.intval, $3.intval,$$.intval);} //ok
	|	F {$$.intval = $1.intval;}
	 ;     //ok e o ; eh para finalizar neste ; Ja que nao tem o OU |

F    : _id {$$.intval=insertSymbTab($1.symbol, Variable); //ok
          }

     | _n {$$.intval=insertSymbTab($1.symbol, Constant); //ok
          }
	;

M   : /* empty */  {$$.intval=prox;
		prox++;
		} 
	;

N   :  /* empty */  {$$.intval=prox;		
		}
	;
%%

/* Comentarios Ag

prox -> indice para a nova entrada, quadrupla
tempo -> endereco temporario na nova tabela
insertSimbtab: endereco do id (indicador de variavel)
			devolve novao na tabela
		busca e inclusao
	end do nome na tabela 

$$ : simbolo na esquerda (campo de atributo)
$1,... : simbolo na direita
tipo YYSTYPE


if->M : guarda posicao na quadrupla que cria sera JF, cond
if cond M then... : quando posicao na tabela e abre espaco
		|->posicao da quadrupla

while -> guarda posicao da quadrupla antes da cond (Jump incondic.)
|-> abre espaco para quadrupla (jump condicional)



COMANDOS:

bison -d trabalho.y
flex trabalho.l
cc lex.yy.c trabalho.tab.c -o trabalho



*/


void atendeReclamacao () {
  int aux;
  aux = 0; // trying avoid compilation error in bison
  }
