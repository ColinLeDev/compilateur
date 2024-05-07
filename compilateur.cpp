//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"

#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <map>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>

using namespace std;

enum OPREL {EQU,	DIFF,	INF,	SUP,	INFE,	SUPE,	WTFR};
enum OPADD {ADD,	SUB,	OR,	WTFA};
enum OPMUL {MUL,	DIV,	MOD,	AND,	WTFM};
enum KEYWORD {IF,	THEN,	ELSE,	WHILE,	DO,	BEGIN,	END,	WTFK}; // UNUSED but useful BEGIN and END are equivalent to { and } in C++
enum TYPES {INTEGER,	BOOLEAN,	UNSIGNED_INT,	WTFT};

TOKEN current; // Current token

FlexLexer *lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

map<string, TYPES> DeclaredVariables;
unsigned long TagNumber = 0;

void Error(string s)
{
	cerr << "Ligne n°" << lexer->lineno() << ", lu : '" << lexer->YYText() << "' (*" << current << "*), mais ";
	cerr << s << endl;
	exit(-1);
}

bool IsDeclared(const char *id)
{
	return DeclaredVariables.find(id) != DeclaredVariables.end();
}

bool IsKeyword(const char *kw)
{
	if (current != KEYWORD)
		return false;
	return (strcmp(lexer->YYText(), kw) == 0);
}

void ReadKeyword(const char *kw)
{
	if (!IsKeyword(kw))
		Error("Mot clé attendu");
	current = (TOKEN)lexer->yylex();
}

TYPES ParseType()
{
	if(current != TYPE)
		Error("Type attendu");
	if (strcmp(lexer->YYText(), "INTEGER") == 0)
		return INTEGER;
	else if (strcmp(lexer->YYText(), "BOOLEAN") == 0)
		return BOOLEAN;
	else if (strcmp(lexer->YYText(), "UNSIGNED_INT") == 0)
		return UNSIGNED_INT;
	else
		Error("Type inconnu");
		return WTFT;
}

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement
// AssignementStatement := Letter "=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"

TYPES Identifier(void)
{
	cout << "\tpush " << lexer->YYText() << endl;
	current = (TOKEN)lexer->yylex();
	return DeclaredVariables[lexer->YYText()];
}

TYPES Number(void)
{
	cout << "\tpush $" << atoi(lexer->YYText()) << endl;
	current = (TOKEN)lexer->yylex();
	return UNSIGNED_INT;
}

TYPES Expression(void); // Called by Term() and calls Term()

TYPES Factor(void)
{
	TYPES type;
	if (current == RPARENT)
	{
		current = (TOKEN)lexer->yylex();
		type = Expression();
		if (current != LPARENT)
			Error("')' était attendu"); // ")" expected
		else
			current = (TOKEN)lexer->yylex();
	}
	else if (current == NUMBER)
		type = Number();
	else if (current == ID)
		type = Identifier();
	else
		Error("'(' ou chiffre ou lettre attendue");
	return type;
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void)
{
	OPMUL opmul;
	if (strcmp(lexer->YYText(), "*") == 0)
		opmul = MUL;
	else if (strcmp(lexer->YYText(), "/") == 0)
		opmul = DIV;
	else if (strcmp(lexer->YYText(), "%") == 0)
		opmul = MOD;
	else if (strcmp(lexer->YYText(), "&&") == 0)
		opmul = AND;
	else
		opmul = WTFM;
	current = (TOKEN)lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPES Term(void)
{
	TYPES type1, type2;
	OPMUL mulop;
	type1 = Factor();
	while (current == MULOP)
	{
		mulop = MultiplicativeOperator(); // Save operator in local variable
		type2 = Factor();
		if (type1 != type2)
		{
			cerr << "Type Factor1:" << type1 << endl
					 << "Type Factor2:" << type2 << endl;
			Error("Les types des opérandes de Factor ne correspondent pas");
		}
		cout << "\tpop %rbx" << endl; // get first operand
		cout << "\tpop %rax" << endl; // get second operand
		switch (mulop)
		{
		case AND:
			cout << "\tmulq	%rbx" << endl;				// a * b -> %rdx:%rax
			cout << "\tpush %rax\t# AND" << endl; // store result
			break;
		case MUL:
			cout << "\tmulq	%rbx" << endl;				// a * b -> %rdx:%rax
			cout << "\tpush %rax\t# MUL" << endl; // store result
			break;
		case DIV:
			cout << "\tmovq $0, %rdx" << endl;		// Higher part of numerator
			cout << "\tdiv %rbx" << endl;					// quotient goes to %rax
			cout << "\tpush %rax\t# DIV" << endl; // store result
			break;
		case MOD:
			cout << "\tmovq $0, %rdx" << endl;		// Higher part of numerator
			cout << "\tdiv %rbx" << endl;					// remainder goes to %rdx
			cout << "\tpush %rdx\t# MOD" << endl; // store result
			break;
		default:
			Error("opérateur multiplicatif attendu");
		}
	}
	return type1;
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void)
{
	OPADD opadd;
	if (strcmp(lexer->YYText(), "+") == 0)
		opadd = ADD;
	else if (strcmp(lexer->YYText(), "-") == 0)
		opadd = SUB;
	else if (strcmp(lexer->YYText(), "||") == 0)
		opadd = OR;
	else
		opadd = WTFA;
	current = (TOKEN)lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
TYPES SimpleExpression(void)
{
	TYPES type1, type2;
	OPADD adop;
	type1 = Term();
	while (current == ADDOP)
	{
		adop = AdditiveOperator(); // Save operator in local variable
		type2 = Term();
		if (type1 != type2)
		{
			cerr << "Type Term1:" << type1 << endl
					 << "Type Term2:" << type2 << endl;
			Error("Les types des opérandes ne correspondent pas");
		}
		cout << "\tpop %rbx" << endl; // get first operand
		cout << "\tpop %rax" << endl; // get second operand
		switch (adop)
		{
		case OR:
			cout << "\taddq	%rbx, %rax\t# OR" << endl; // operand1 OR operand2
			break;
		case ADD:
			cout << "\taddq	%rbx, %rax\t# ADD" << endl; // add both operands
			break;
		case SUB:
			cout << "\tsubq	%rbx, %rax\t# SUB" << endl; // substract both operands
			break;
		default:
			Error("opérateur additif inconnu");
		}
		cout << "\tpush %rax" << endl; // store result
	}
	return type1;
}
// VarDeclaration := Type Ident {"," Ident}
void VarDeclaration(void)
{
	TYPES type = ParseType();
	do
	{
		current = (TOKEN)lexer->yylex();	// Consommer le type, puis les virgules
		if (current != ID)
			Error("Identificateur attendu");
		if (IsDeclared(lexer->YYText()))
		{
			cerr << "Variable '" << lexer->YYText() << "'" << endl;
			Error("Variable déjà déclarée");
		}
		DeclaredVariables[lexer->YYText()] = type;
		current = (TOKEN)lexer->yylex(); // Consommer l'identificateur
	} while (current == COMMA);
}

// DeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void DeclarationPart(void)
{
	cout << "\t.data" << endl;
	cout << "\t.align 8" << endl;
	if (!IsKeyword("VAR"))
		Error("Mot clé 'VAR' attendu");
	do
	{
		current = (TOKEN)lexer->yylex(); // Consommer le mot clé "VAR", puis ";"
		VarDeclaration();
	} while (current==SEMICOLON);
	if (current != DOT)
		Error("caractère '.' attendu");
	current = (TOKEN)lexer->yylex();
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="
OPREL RelationalOperator(void)
{
	OPREL oprel;
	if (strcmp(lexer->YYText(), "==") == 0)
		oprel = EQU;
	else if (strcmp(lexer->YYText(), "!=") == 0)
		oprel = DIFF;
	else if (strcmp(lexer->YYText(), "<") == 0)
		oprel = INF;
	else if (strcmp(lexer->YYText(), ">") == 0)
		oprel = SUP;
	else if (strcmp(lexer->YYText(), "<=") == 0)
		oprel = INFE;
	else if (strcmp(lexer->YYText(), ">=") == 0)
		oprel = SUPE;
	else
		oprel = WTFR;
	current = (TOKEN)lexer->yylex();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPES Expression(void)
{
	TYPES type1, type2;
	OPREL oprel;
	type1 = SimpleExpression();
	if (current == RELOP)
	{
		oprel = RelationalOperator();
		type2 = SimpleExpression();
		if (type1 != type2)
		{
			cerr << "Type SimpleExpression1:" << type1 << endl
					 << "Type SimpleExpression2:" << type2 << endl;
			Error("Les types des opérandes ne correspondent pas");
		}
		cout << "\tpop %rax" << endl;
		cout << "\tpop %rbx" << endl;
		cout << "\tcmpq %rax, %rbx" << endl;
		switch (oprel)
		{
		case EQU:
			cout << "\tje Vrai" << ++TagNumber << "\t# If equal" << endl;
			break;
		case DIFF:
			cout << "\tjne Vrai" << ++TagNumber << "\t# If different" << endl;
			break;
		case SUPE:
			cout << "\tjae Vrai" << ++TagNumber << "\t# If above or equal" << endl;
			break;
		case INFE:
			cout << "\tjbe Vrai" << ++TagNumber << "\t# If below or equal" << endl;
			break;
		case INF:
			cout << "\tjb Vrai" << ++TagNumber << "\t# If below" << endl;
			break;
		case SUP:
			cout << "\tja Vrai" << ++TagNumber << "\t# If above" << endl;
			break;
		default:
			Error("Opérateur de comparaison inconnu");
		}
		cout << "\tpush $0\t\t# False" << endl;
		cout << "\tjmp Suite" << TagNumber << endl;
		cout << "Vrai" << TagNumber << ":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True" << endl;
		cout << "Suite" << TagNumber << ":" << endl;
		return BOOLEAN;
	}
	return type1;
}

// AssignementStatement := Identifier ":=" Expression
string AssignementStatement(void)
{
	string variable;
	if (current != ID)
		Error("Identificateur attendu");
	if (!IsDeclared(lexer->YYText()))
	{
		cerr << "Erreur : Variable '" << lexer->YYText() << "' non déclarée" << endl;
		exit(-1);
	}
	variable = lexer->YYText();
	current = (TOKEN)lexer->yylex();
	if (current != ASSIGN)
		Error("caractères ':=' attendus");
	current = (TOKEN)lexer->yylex();
	TYPES type = Expression();
	if(type != DeclaredVariables[variable]){
		cerr << "Type de la variable '" << variable << "' : "<< DeclaredVariables[variable] << endl;
		cerr << "Tentative d'affectation : "<< type << endl;
		Error("Tentative d'affectation d'un type incompatible");
	}
	cout << "\tpop " << variable << endl;
	return variable;
}

void Statement(void);

// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
void BlockStatement(void)
{
	ReadKeyword("BEGIN");
	Statement();
	while (current == SEMICOLON)
	{
		current = (TOKEN)lexer->yylex();
		Statement();
	}
	ReadKeyword("END"); // Lire le "END"
}

// IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
void IfStatement(void)
{
	ReadKeyword("IF"); // Lire le "IF"
	int localTag = ++TagNumber;

	cout << "IF" << localTag << ":" << endl;
	TYPES type = Expression();
	if (type != BOOLEAN)
	{
		cerr << "Type de l'expression : " << type << endl;
		Error("L'expression dans le IF doit être de type BOOLEAN");
	}
	
	ReadKeyword("THEN"); // Lire le "THEN"
	cout << "IFCHECK" << localTag << ":\t popq %rax" << endl;
	cout << "\tcmpq $0, %rax" << endl;
	cout << "\tje ELSE" << localTag << ":" << endl;
	cout << "IFTHEN" << localTag << ":" << endl;
	Statement();
	cout << "\tjmp IFEND" << localTag << endl;
	cout << "IFELSE" << localTag << ":" << endl;
	if (current == KEYWORD && strcmp(lexer->YYText(), "ELSE") == 0)
	{
		current = (TOKEN)lexer->yylex(); // Lire le "ELSE"
		Statement();
	}
	cout << "IFEND" << localTag << ":" << endl;
}

// WhileStatement := "WHILE" Expression "DO" Statement
void WhileStatement(void)
{
	if (current != KEYWORD || !(strcmp(lexer->YYText(), "WHILE") == 0))
	{
		Error("Mot clé 'WHILE' attendu");
	}
	current = (TOKEN)lexer->yylex(); // Lire le "WHILE"
	int localTag = ++TagNumber;
	cout << "While" << localTag << ":" << endl;
	TYPES type = Expression();
	if (type != BOOLEAN)
	{
		cerr << "Type de l'expression : " << type << endl;
		Error("L'expression dans le WHILE doit être de type BOOLEAN");
	}
	cout << "WhileCheck" << localTag << ":\t popq %rax" << endl;
	cout << "\tcmpq $0, %rax" << endl;
	cout << "\tje WhileEnd" << localTag << endl;
	if (current != KEYWORD || !(strcmp(lexer->YYText(), "DO") == 0))
	{
		Error("Mot clé 'DO' attendu");
	}
	current = (TOKEN)lexer->yylex(); // Lire le "DO"
	Statement();
	cout << "\tjmp While" << localTag << endl;
	cout << "WhileEnd" << localTag << ":" << endl;
}

// ForStatement := "FOR" Identifier ":=" Expression "TO" Expression "DO" Statement
void ForStatement(void)
{
	ReadKeyword("FOR");
	if (current != ID)
	{
		Error("Identificateur attendu");
	}
	int localTag = ++TagNumber;
	cout << "ForAssign" << localTag << ":" << endl;
	string varBoucle = AssignementStatement();
	ReadKeyword("TO");
	cout << "ForTo" << localTag << ":" << endl;
	Expression();
	ReadKeyword("DO");
	cout << "ForTest" << localTag << ":\t movq (%rsi), %rax" << endl;
	cout << "\tcmpq %rax, " << varBoucle << endl;
	cout << "\tjb ForEnd" << localTag << "\t# si varBoucle < i" << endl;
	Statement();
	cout << "ForInc" << localTag << ":" << endl;
	cout << "\tincq " << varBoucle << endl;
	cout << "\tjmp ForTest" << localTag << endl;
	cout << "ForEnd" << localTag << ":" << endl;
}

// Keyword := "IF" | "THEN" | "ELSE" | "WHILE" | "DO" | "BEGIN" | "END"
// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
void Statement(void)
{
	if (current == KEYWORD)
	{
		if (strcmp(lexer->YYText(), "IF") == 0)
		{
			IfStatement();
		}
		else if (strcmp(lexer->YYText(), "WHILE") == 0)
		{
			WhileStatement();
		}
		else if (strcmp(lexer->YYText(), "FOR") == 0)
		{
			ForStatement();
		}
		else if (strcmp(lexer->YYText(), "BEGIN") == 0)
		{
			BlockStatement();
		}
		else
			Error("Mot clé inattendu à cet endroit, mot clé d'ouverture attendu");
	}
	else if (current == ID)
	{
		AssignementStatement();
	}
	else
		Error("Une instruction était attendue.");
}

// StatementPart := Statement {";" Statement} "."
void StatementPart(void)
{
	cout << "\t.text\t\t# The following lines contain the program" << endl;
	cout << "\t.globl main\t# The main function must be visible from outside" << endl;
	cout << "main:\t\t\t# The main function body :" << endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top" << endl;
	Statement();
	while (current == SEMICOLON)
	{
		current = (TOKEN)lexer->yylex();
		Statement();
	}
	if (current != DOT)
		Error("caractère '.' attendu");
	current = (TOKEN)lexer->yylex();
}

// Program := [DeclarationPart] StatementPart
void Program(void)
{
	if (IsKeyword("VAR"))
		DeclarationPart();
	StatementPart();
}

int main(void)
{ // First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler" << endl;
	// Let's proceed to the analysis and code production
	current = (TOKEN)lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top" << endl;
	cout << "\tret\t\t\t# Return from main function" << endl;
	if (current != FEOF)
	{
		cerr << "Caractères en trop à la fin du programme : [" << current << "]";
		Error("."); // unexpected characters at the end of program
	}
}
