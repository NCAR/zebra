%{

# include "DerivTable.h"
# include "DerivNode.h"
# include "Field.h"

/*
 * Pointer to the derivation table we're to modify.  This must be set by 
 * the caller before FDparse() is called!
 */
DerivTable	*FDTable = 0;

extern "C" char *gettxt( const char*, const char * );

int yylex( void );

void
yyerror(const char* errmsg)
{
	printf( "%s\n", errmsg );
}
%}

%union {
	char	str[128];
	double	val;
	Field*	fld;
	DerivNode*	dnode;
}

%token <str> NAME	/* [a-zA-Z_][a-zA-Z0-9_\$.-]* */
%token <str> BSTRING	/* any string enclosed in brackets ([]) */
%token <val> DOUBLE

%type <fld> field
%type <dnode> expr

/*
 * Establish standard operator precedence
 */
%left '<' '>' EQ NE LE GE
%left '+' '-'
%left '*' '/'

%%

list:	/* empty */
	| derivation list
	;

derivation:	
	field '=' expr ';'	{ FDTable->AddDerivation(*($1), $3); 
				  delete $1; }
	| error ';'
	;

field:	NAME			{ $$ = new Field($1); }
	| NAME BSTRING		{ $$ = new Field($1, $2); }
	| NAME BSTRING BSTRING	{ $$ = new Field($1, $2, $3); }
	| NAME BSTRING BSTRING BSTRING	{ $$ = new Field($1, $2, $3, $4); }
	| BSTRING		{ $$ = new Field(0, $1); }
	| BSTRING BSTRING	{ $$ = new Field(0, $1, $2); }
	| BSTRING BSTRING BSTRING	{ $$ = new Field(0, $1, $2, $3); }
	;

expr:	field			{ $$ = new RawFldDNode($1); }
	| DOUBLE		{ $$ = new ConstDNode($1); }
	| '(' expr ')'		{ $$ = $2; }
	| '(' error ')'		{ $$ = 0; }
	| NAME '(' expr ')'	{ $$ = new FuncDNode($1, $3); }
	| NAME '(' expr ',' expr ')'	
				{ $$ = new FuncDNode($1, $3, $5); }
	| NAME '(' expr ',' expr ',' expr ')'	
				{ $$ = new FuncDNode($1, $3, $5, $7); }
	| NAME '(' expr ',' expr ',' expr ',' expr ')'	
				{ $$ = new FuncDNode($1, $3, $5, $7, $9); }
	| NAME '(' error ')'	{ $$ = 0; }
	| expr '+' expr		{ $$ = new OpDNode("+", $1, $3); }
	| expr '-' expr		{ $$ = new OpDNode("-", $1, $3); }
	| '+' expr		{ $$ = $2; }
	| '-' expr	{ $$ = new OpDNode("*", new ConstDNode(-1), $2); }
	| expr '*' expr		{ $$ = new OpDNode("*", $1, $3); }
	| expr '/' expr		{ $$ = new OpDNode("/", $1, $3); }
	| expr '<' expr		{ $$ = new OpDNode("<", $1, $3); }
	| expr '>' expr		{ $$ = new OpDNode(">", $1, $3); }
	| expr EQ expr		{ $$ = new OpDNode("==", $1, $3); }
	| expr NE expr		{ $$ = new OpDNode("!=", $1, $3); }
	| expr GE expr		{ $$ = new OpDNode(">=", $1, $3); }
	| expr LE expr		{ $$ = new OpDNode("<=", $1, $3); }
	;

%%
