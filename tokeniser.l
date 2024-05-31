%{
// This is our Lexical tokeniser 
// It should be compiled into cpp with :
// flex++ -d -otokeniser.cpp tokeniser.l 
// And then compiled into object with
// g++ -c tokeniser.cpp
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string
//{stringconst}	return STRINGCONST;

#include "tokeniser.h"
#include <iostream>

using namespace std;

%}

%option noyywrap
%option c++
%option yylineno

stringconst  \"[^\n"]+\"
charconst  \'[^\n']\'
ws      [ \t\n\r]+
alpha   [A-Za-z]
digit   [0-9]
number  {digit}+(\.{digit})?
id	{alpha}({alpha}|{digit})*
addop	(\+|\-|\|\|)
mulop	(\*|\/|%|\&\&)
relop	(\<|\>|"=="|\<=|\>=|!=)
keyword (IF|THEN|ELSE|WHILE|DO|FOR|TO|DO|BEGIN|END|VAR|DISPLAY|CASE|OF|REPEAT|UNTIL)
types (INTEGER|BOOLEAN|DOUBLE|CHAR)
boolconst (TRUE|FALSE)
unknown [^\"A-Za-z0-9 \n\r\t\(\)\<\>\=\!\%\&\|\}\-\;\.\:]+

%%

{addop}		return ADDOP;
{mulop}		return MULOP;
{relop}		return RELOP;
{boolconst} return BOOLCONST;
{number}	return NUMBER;
{types}	return TYPE;
{keyword}	return KEYWORD;
{id}		return ID;
{charconst}	return CHARCONST;
"["		return RBRACKET;
"]"		return LBRACKET;
","		return COMMA;
";"		return SEMICOLON;
":"		return COLON;
"."		return DOT;
":="		return ASSIGN;
"("		return RPARENT;
")"		return LPARENT;
"!"		return NOT;
<<EOF>>		return FEOF;
{ws}    {/* skip blanks and tabs */};
"(*"    { /* Skip comments between '(*' and '*)' */
		int c;
		while((c = yyinput()) != 0){
     			if(c == '*'){
     	    			if((c = yyinput()) == ')')
    	        			break;
    	     			else
  	          			unput(c);
  	   		}	
		}
	};

{unknown}	return UNKNOWN;

%%

