/*
 * Token definitions for field derivations
 */
%{
#include <math.h>	/* for strtod() used below */
#include <string.h>	/* for strcpy() */

#include "DerivNode.h"
#include "y.tab.h"
 
#define yywrap()	1	
%}

/*
 * Parsing contexts for bracketed strings and comments
 */
%s bstring comment

NUMBER		[0-9]+
SIGNED_NUM	[+-]?{NUMBER}
DECIMAL		({SIGNED_NUM}[\.]?)|([+-]?{NUMBER}?\.{NUMBER})
EDECIMAL	{DECIMAL}|({DECIMAL}[eE]{SIGNED_NUM})

%%

[ \t\n]+			/* munch white space */;

"/*"				{
				/*
				 * Skip to the end of the comment
				 */
					char	c;

					for(;;)
					{
						while (yyinput() != '*')
							/* nothing */;

						if ((c = yyinput()) == '/')
							break;

						unput(c);
					}
				}


[a-zA-Z_][a-zA-Z0-9_\$.-]*	{
					strncpy(yylval.str, yytext, yyleng);
					yylval.str[yyleng] = 0;
					return NAME;
				}


{EDECIMAL}			{ 
					yylval.val = strtod(yytext, 0); 
					return DOUBLE;
				}


"["[^\]]*"]"			{
					int len;
					strncpy( yylval.str, yytext, yyleng );
					yylval.str[yyleng] = 0;
				/*
				 * Lose the brackets enclosing our 
				 * string and terminate it with
				 * a null.
				 */
					len = strlen(yylval.str);
# ifdef __STDC__
					memmove (yylval.str, yylval.str+1, 
						 len-2);
# else
					void bcopy(char *src, char* dest, 
						   int len);
					bcopy (str+1, str, len-2);
# endif
					yylval.str[len-2] = 0;
					return BSTRING;
				}

"!="				return NE;
">="				return GE;
"<="				return LE;
"=="				return EQ;
"("				return '(';
")"				return ')';
"<"				return '<';
">"				return '>';
"="				return '=';
"+"				return '+';
"-"				return '-';
"*"				return '*';
"/"				return '/';
";"				return ';';
","				return ',';


