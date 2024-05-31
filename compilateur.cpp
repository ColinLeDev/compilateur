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
#include <cstring>
#include <FlexLexer.h>
#include "tokeniser.h"

using namespace std;

TOKEN current; // Current token

FlexLexer *lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

unsigned long TagNumber = 0;

// ENUMS

enum TYPES
{
	INTEGER,
	BOOLEAN,
	DOUBLE,
	CHAR,
	WTFT
};
enum OPREL
{
	EQU,
	DIFF,
	INF,
	SUP,
	INFE,
	SUPE,
	WTFR
};
enum OPADD
{
	ADD,
	SUB,
	OR,
	WTFA
};
enum OPMUL
{
	MUL,
	DIV,
	MOD,
	AND,
	WTFM
};
enum KEYWORD
{
	IF,
	THEN,
	ELSE,
	WHILE,
	DO,
	BEGIN,
	END,
	WTFK
}; // UNUSED but useful BEGIN and END are equivalent to { and } in C++

map<string, TYPES> DeclaredVariables;

// UTILS

const char *charTokenToString(TOKEN token)
{
	switch (token)
	{
	case FEOF:
		return "FEOF";
	case UNKNOWN:
		return "UNKNOWN";
	case NUMBER:
		return "NUMBER";
	case ID:
		return "ID";
	case STRINGCONST:
		return "STRINGCONST";
	case RBRACKET:
		return "RBRACKET";
	case LBRACKET:
		return "LBRACKET";
	case RPARENT:
		return "RPARENT";
	case LPARENT:
		return "LPARENT";
	case COMMA:
		return "COMMA";
	case SEMICOLON:
		return "SEMICOLON";
	case DOT:
		return "DOT";
	case ADDOP:
		return "ADDOP";
	case MULOP:
		return "MULOP";
	case RELOP:
		return "RELOP";
	case NOT:
		return "NOT";
	case ASSIGN:
		return "ASSIGN";
	case KEYWORD:
		return "KEYWORD";
	case TYPE:
		return "TYPE";
	case COLON:
		return "COLON";
	case CHARCONST:
		return "CHARCONST";
	case BOOLCONST:
		return "BOOLCONST";
	default:
		return "UNKNOWN";
	}
}
const char *charTypeToString(TYPES t)
{
	switch (t)
	{
	case INTEGER:
		return "INTEGER";
	case BOOLEAN:
		return "BOOLEAN";
	case DOUBLE:
		return "DOUBLE";
	case CHAR:
		return "CHAR";
	default:
		return "WTFT";
	}
}

// Check if the type is the expected one, allowinf a third argument to be tested
bool CheckType(TYPES t1, TYPES t2, TYPES t3 = WTFT)
{
	return t1 == t2
				 // If t3 defined, test it
				 && (t3 == WTFT || t1 == t3);
}

// ERROR ET WARNING

void Error(string s)
{
	cerr << "Error :" << endl;
	cerr << "Ligne n°" << lexer->lineno() << ", lu : '" << lexer->YYText() << "' (*" << charTokenToString(current) << "*), mais ";
	cerr << s << endl;
	exit(-1);
}
void Warning(string s)
{
	cerr << "⚠️  Ligne n°" << lexer->lineno() << " : ";
	cerr << s << endl;
}

// UTILS FOR LEXER

// Compare the current token with a string
bool ccmp(const char *str)
{
	return strcmp(lexer->YYText(), str) == 0;
}

void Read()
{
	current = (TOKEN)lexer->yylex();
}

// Avancer la tête de lecture si le token correspond, et renvoie un booléen indiquant le succès ou non de l'opération
bool ReadIf(TOKEN t)
{
	if (current == t)
	{
		Read();
		return true;
	}
	return false;
}

bool IsDeclared(const char *id)
{
	return DeclaredVariables.find(id) != DeclaredVariables.end();
}

bool IsKeyword(const char *kw)
{
	if (current != KEYWORD)
		return false;
	return (ccmp(kw));
}

void ReadKeyword(const char *kw)
{
	if (!IsKeyword(kw))
		Error("Mot clé attendu");
	Read();
}

// CODE

TYPES ParseType(void)
{
	if (current != TYPE)
		Error("Type attendu");
	if (ccmp("INTEGER"))
	{
		Read();
		return INTEGER;
	}
	else if (ccmp("BOOLEAN"))
	{
		Read();
		return BOOLEAN;
	}
	else if (ccmp("DOUBLE"))
	{
		Read();
		return DOUBLE;
	}
	else if (ccmp("CHAR"))
	{
		Read();
		return CHAR;
	}
	else
	{
		cerr << "Type : " << lexer->YYText();
		Error("Type inconnu");
	}
	return WTFT;
}

// Program := [DeclarationPart] StatementPart
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
	TYPES type = DeclaredVariables[lexer->YYText()];
	Read();
	return type;
}

TYPES Number(void)
{
	TYPES type;
	string num = lexer->YYText();
	double d;													 // 64-bit float
	unsigned long long *l;						 // Pointer to 64-bit unsigned integer
	if (num.find('.') != string::npos) // Token is a DOUBLE
	{
		d = stod(num);								// Convert string TOKEN to double
		l = (unsigned long long *)&d; // Get the address of the double
		unsigned int high_part = *l;
		unsigned int low_part = *(((unsigned int *)l) + 1);
		cout << "\tsubq\t$8, %rsp\t\t\t# Going to add on stack top" << endl;
		cout << "\tmovl\t$" << high_part << ", (%rsp)\t# 32-bit high part of " << d << endl;
		cout << "\tmovl\t$" << low_part << ", 4(%rsp)\t# 32-bit low part of " << d << endl;
		type = DOUBLE;
	}
	else
	{
		cout << "\tpush $" << atoi(lexer->YYText()) << endl;
		type = INTEGER;
	}
	Read();
	return type;
}

// BoolConst := "TRUE" | "FALSE"
TYPES BoolConst(void)
{
	cout << "\tpush $" << (ccmp("TRUE") ? -1 : 0) << endl;
	Read();
	return BOOLEAN;
}

// CharConst := "'" Letter "'"
TYPES CharConst(void)
{
	cout << "\tmovq\t$0, %rax" << endl;
	cout << "\tmovb\t$" << (int)lexer->YYText()[1] << ", %al" << endl;
	cout << "\tpush\t%rax\t# push a 8-byte version of " << lexer->YYText() << endl;
	Read();
	return CHAR;
}

TYPES Expression(void); // Called by Term() and calls Term()

TYPES Factor(void)
{
	TYPES type;
	if (current == RPARENT)
	{
		Read();
		type = Expression();
		if (current != LPARENT)
			Error("')' était attendu"); // ")" expected
		else
			Read();
	}
	else if (current == NUMBER)
		type = Number();
	else if (current == ID)
		type = Identifier();
	else if (current == BOOLCONST)
		type = BoolConst();
	else if (current == CHARCONST)
		type = CharConst();
	else
		Error("'(' ou variable ou constante attendue");
	return type;
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void)
{
	OPMUL opmul;
	if (ccmp("*"))
		opmul = MUL;
	else if (ccmp("/"))
		opmul = DIV;
	else if (ccmp("%"))
		opmul = MOD;
	else if (ccmp("&&"))
		opmul = AND;
	else
		opmul = WTFM;
	Read();
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
		switch (mulop)
		{
		case AND:
			CheckType(type1, BOOLEAN);			// Only on bool, to prevent unexpected behavior with double
			cout << "\tmulq  %rbx" << endl; // a * b -> %rdx:%rax
			break;
		case MUL:
			switch (type1)
			{
			case DOUBLE:
				cout << "\tfldl (%rsp)" << endl;
				cout << "\taddq $8, %rsp\t# popq" << endl;
				cout << "\tfldl (%rsp)" << endl;
				cout << "\t# addq $8, %rsp\t# not popq : result will be stored" << endl;
				cout << "\tfmul %st(1)" << endl;
				cout << "\tfstpl (%rsp)\t# retrieve st0" << endl; // store result
				cout << "\tfmulp %st(0)\t# Depop" << endl;
				break;

			case INTEGER:
			default:
				cout << "\tpop %rbx" << endl; // get first operand
				cout << "\tpop %rax" << endl; // get second operand
				cout << "\tmulq  %rbx" << endl; // a * b -> %rdx:%rax
				cout << "\tpush %rax\t# MUL" << endl; // store result
				break;
			}
			break;
		case DIV:
			switch (type1)
			{
			case DOUBLE:
				cout << "\tfldl (%rsp)" << endl;
				cout << "\taddq $8, %rsp\t# popq" << endl;
				cout << "\tfldl (%rsp)" << endl;
				cout << "\t# addq $8, %rsp\t# not popq : result will be stored" << endl;
				cout << "\tfdivp %st(0) %st(1)\t# Store in st1, pop, so stocked in st0" << endl;
				cout << "\tfstpl (%rsp)\t# retrieve st0, no %rsp change" << endl; // store result
				break;

			case INTEGER:
			default:
				cout << "\tpop %rbx" << endl; // get first operand
				cout << "\tpop %rax" << endl; // get second operand
				cout << "\tmovq $0, %rdx" << endl; // Higher part of numerator
				cout << "\tdiv %rbx" << endl;			 // quotient goes to %rax
				cout << "\tpush %rax\t# DIV" << endl; // store result
				break;
			}
			break;
		case MOD:
			if (type1 == DOUBLE)
			{
				Error("Modulo operation isn't allowed on double");
			}
			cout << "\tpop %rbx" << endl; // get first operand
			cout << "\tpop %rax" << endl; // get second operand
			cout << "\tmovq $0, %rdx" << endl;					// Higher part of numerator
			cout << "\tdiv %rbx" << endl;								// remainder goes to %rdx
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
	if (ccmp("+"))
		opadd = ADD;
	else if (ccmp("-"))
		opadd = SUB;
	else if (ccmp("||"))
		opadd = OR;
	else
		opadd = WTFA;
	Read();
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
			cerr << "Type Term1:" << charTypeToString(type1) << endl
					 << "Type Term2:" << charTypeToString(type2) << endl;
			Error("Les types des opérandes ne correspondent pas");
		}
		cout << "\tpop %rbx" << endl; // get first operand
		cout << "\tpop %rax" << endl; // get second operand
		switch (adop)
		{
		case OR:
			CheckType(type1, BOOLEAN);
			cout << "\taddq  %rbx, %rax\t# OR" << endl; // operand1 OR operand2
			break;
		case ADD:
			cout << "\taddq  %rbx, %rax\t# ADD" << endl; // add both operands
			break;
		case SUB:
			cout << "\tsubq  %rbx, %rax\t# SUB" << endl; // substract both operands
			break;
		default:
			Error("opérateur additif inconnu");
		}
		cout << "\tpush %rax" << endl; // store result
	}
	return type1;
}
// VarDeclaration := Ident {"," Ident} ":" Type
void VarDeclaration(void)
{
	set<string> Identifiers;
	do
	{
		ReadIf(COMMA); // Consommer la virgule
		if (current != ID)
			Error("Identificateur attendu");
		if (IsDeclared(lexer->YYText()))
		{
			cerr << "Variable " << lexer->YYText() << "" << endl;
			Error("Variable déjà déclarée");
		}
		if (Identifiers.find(lexer->YYText()) != Identifiers.end())
		{
			cerr << "Variable " << lexer->YYText() << "" << endl;
			Warning("Variable doublement déclarée dans le type");
		}
		Identifiers.insert(lexer->YYText());
		Read(); // Consommer l'identificateur
	} while (current == COMMA);
	TYPES type;
	if (!ReadIf(COLON))
		Error("':' attendu");
	type = ParseType();
	const char *typeString = charTypeToString(type);
	for (set<string>::iterator it = Identifiers.begin(); it != Identifiers.end(); ++it)
	{
		DeclaredVariables[*it] = type;
		switch (type)
		{
		case INTEGER:
		case BOOLEAN:
			cout << *it << ":\t.quad 0 # " << typeString << endl;
			break;
		case DOUBLE:
			cout << *it << ":\t.double 0.0 # " << typeString << endl;
			break;
		case CHAR:
			cout << *it << ":\t.byte 0 # " << typeString << endl;
			break;
		default:
			cerr << "Type : " << typeString << endl;
			Error("Une variable de ce type ne peut pas être déclarée");
		}
	}
}

// VarDeclarationPart  := "VAR" VarDeclaration {";" VarDeclaration} "."
void VarDeclarationPart(void)
{
	ReadKeyword("VAR");
	do
	{
		ReadIf(SEMICOLON); // Consommer ";"
		VarDeclaration();
	} while (current == SEMICOLON);
	if (current != DOT)
		Error("caractère '.' attendu");
	Read();
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="
OPREL RelationalOperator(void)
{
	OPREL oprel;
	if (ccmp("=="))
		oprel = EQU;
	else if (ccmp("!="))
		oprel = DIFF;
	else if (ccmp("<"))
		oprel = INF;
	else if (ccmp(">"))
		oprel = SUP;
	else if (ccmp("<="))
		oprel = INFE;
	else if (ccmp(">="))
		oprel = SUPE;
	else
		oprel = WTFR;
	Read();
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
			cout << "\tje _Vrai" << ++TagNumber << "\t# If equal" << endl;
			break;
		case DIFF:
			cout << "\tjne _Vrai" << ++TagNumber << "\t# If different" << endl;
			break;
		case SUPE:
			cout << "\tjae _Vrai" << ++TagNumber << "\t# If above or equal" << endl;
			break;
		case INFE:
			cout << "\tjbe _Vrai" << ++TagNumber << "\t# If below or equal" << endl;
			break;
		case INF:
			cout << "\tjb _Vrai" << ++TagNumber << "\t# If below" << endl;
			break;
		case SUP:
			cout << "\tja _Vrai" << ++TagNumber << "\t# If above" << endl;
			break;
		default:
			Error("Opérateur de comparaison inconnu");
		}
		cout << "\tpush $0\t\t# False" << endl;
		cout << "\tjmp _Suite" << TagNumber << endl;
		cout << "_Vrai" << TagNumber << ":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True" << endl;
		cout << "_Suite" << TagNumber << ":" << endl;
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
	Read();
	if (current != ASSIGN)
		Error("caractères ':=' attendus");
	Read();
	TYPES type = Expression();
	if (type != DeclaredVariables[variable])
	{
		cerr << "Type de la variable '" << variable << "' : " << charTypeToString(DeclaredVariables[variable]) << endl;
		cerr << "Tentative d'affectation : " << charTypeToString(type) << endl;
		Error("Tentative précédente d'affectation d'un type incompatible");
	}
	if(type == CHAR)
	{
		cout << "\tpop %rax" << endl;
		cout << "\tmovb %al, " << variable << endl;
	}
	else // All other types can be treated in the same way (8 bytes on stack)
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
		Read();
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
		cerr << "Type de l'expression : " << charTypeToString(type) << endl;
		Error("l'expression dans le IF doit être de type BOOLEAN");
	}

	ReadKeyword("THEN"); // Lire le "THEN"
	cout << "IFCHECK" << localTag << ":\t popq %rax" << endl;
	cout << "\tcmpq $0, %rax" << endl;
	cout << "\tje IFELSE" << localTag << endl;
	cout << "IFTHEN" << localTag << ":" << endl;
	Statement();
	cout << "\tjmp IFEND" << localTag << endl;
	cout << "IFELSE" << localTag << ":" << endl;
	if (IsKeyword("ELSE"))
	{
		ReadKeyword("ELSE");
		Statement();
	}
	cout << "IFEND" << localTag << ":" << endl;
}

// WhileStatement := "WHILE" Expression "DO" Statement
void WhileStatement(void)
{
	if (!IsKeyword("WHILE"))
	{
		Error("Mot clé 'WHILE' attendu");
	}
	ReadKeyword("WHILE");
	int localTag = ++TagNumber;
	cout << "While" << localTag << ":" << endl;
	TYPES type = Expression();
	if (!CheckType(type, BOOLEAN))
	{
		cerr << "Type de l'expression : " << charTypeToString(type) << endl;
		Error("L'expression dans le WHILE doit être de type BOOLEAN");
	}
	cout << "WhileCheck" << localTag << ":\t popq %rax" << endl;
	cout << "\tcmpq $0, %rax" << endl;
	cout << "\tje WhileEnd" << localTag << endl;
	if (!IsKeyword("DO"))
	{
		Error("Mot clé 'DO' attendu");
	}
	Read(); // Lire le "DO"
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
	cout << "ForTo" << localTag << ":" << endl;
	bool isTo = IsKeyword("TO");
	if (isTo)
		ReadKeyword("TO");
	else
		ReadKeyword("DOWNTO");
	Expression();
	cout << "ForTest" << localTag << ":\t movq (%rsp), %rax" << endl;
	cout << "\tcmpq %rax, " << varBoucle << endl;
	if (isTo)
		cout << "\tja ForEnd" << localTag << "\t# si varBoucle > i" << endl;
	else
		cout << "\tjb ForEnd" << localTag << "\t# si varBoucle < i" << endl;
	ReadKeyword("DO");
	Statement();
	cout << "ForInc" << localTag << ":" << endl;
	cout << "\tincq " << varBoucle << endl;
	cout << "\tjmp ForTest" << localTag << endl;
	cout << "ForEnd" << localTag << ":" << endl;
}

// CaseLabelList := Factor {"," Factor}
void CaseLabelList(unsigned long localTag, unsigned long caseTag, TYPES expType)
{
	TYPES type;
	do
	{
		ReadIf(COMMA);
		type = Factor();
		if (type != expType)
			Error("Les types des labels du CASE ne correspondent pas avec l'expression");
		switch (type)
		{
		case BOOLEAN:
			Warning("Using BOOLEAN in CASE isn't recommended, use IF statement instead.");
		case INTEGER:
		case CHAR: // CHAR is 1-byte long BUT stored in 8-byte in register (treatable as INTEGER)
			cout << "\tpopq %rax \t# The value to be compared" << endl;
			cout << "\tmovq (%rsp) %rbx \t# The value in the CASE _ OF" << endl;
			cout << "\tcmpq %rax, %rbx" << endl;
			cout << "\tje Case" << localTag << "Label" << caseTag << endl;
			break;

		case DOUBLE:
			cout << "\tfldl (%rsp) \t# The value to be compared" << endl;
			cout << "\taddq $8, %rsp\t# popq" << endl;
			cout << "\tfldl (%rsp) \t# The value in the CASE _ OF" << endl; // ICI pas de popq car on ne veut pas supprimer la valeur
			cout << "\tfcompi %rax, %rbx" << endl;
			cout << "\tje Case" << localTag << "Label" << caseTag << endl;
			break;

		default:
			cerr << "Type : " << charTypeToString(type);
			Error("Type inconnu pour le CASE");
			break;
		}
	} while (current == COMMA);
}

// CaseElement := CaseLabelList ":" Statement
void CaseElement(unsigned long localTag, unsigned long caseTag, TYPES type)
{
	cout << "Case" << localTag << "Test" << caseTag << ":" << endl;
	CaseLabelList(localTag, caseTag, type);
	if (current != COLON)
		Error("':' attendu");
	Read();
	cout << "\tjmp Case" << localTag << "Test" << caseTag + 1 << endl;
	cout << "Case" << localTag << "Label" << caseTag << ":" << endl;

	Statement();
}

// CaseStatement := "CASE" Expression "OF" CaseElement {; CaseElement} ["ELSE" Statement] "END"
void CaseStatement()
{
	ReadKeyword("CASE");
	unsigned long localTag = ++TagNumber, caseTag = 1;
	TYPES type = Expression();

	ReadKeyword("OF");
	do
	{
		ReadIf(SEMICOLON);
		CaseElement(localTag, caseTag++, type);
	} while (current == SEMICOLON);
	cout << "Case" << localTag << "Test" << ++caseTag << ":" << endl;
	if(IsKeyword("ELSE"))
	{
		ReadKeyword("ELSE");
		Statement();
	}
	ReadKeyword("END");
	cout << "\tjmp Case" << localTag << "End" << endl;
	cout << "Case" << localTag << "End:" << endl;
}

// DisplayStatement := "DISPLAY" Expression {"," Expression}
void DisplayStatement(void)
{
	ReadKeyword("DISPLAY");
	TYPES type;
	int localTag = 0, displayNumber = ++TagNumber;
	cout << "Display" << displayNumber << ":" << endl;
	do
	{
		ReadIf(COMMA);
		if (ReadIf(NOT))
		{
			cout << "\tmovq $_EmptyString, %rdi" << endl;
			cout << "\tcall puts@PLT" << endl; // Saut de ligne
			cerr << "Next : " << charTokenToString(current) << endl;
			continue;
		}
		type = Expression();
		cout << "\tmovq $0, %rax" << endl;
		switch (type)
		{
		case INTEGER:
			cout << "\tpop %rsi \t# The value to be displayed" << endl;
			cout << "\tmovq $_FormatStringInt, %rdi" << endl;
			cout << "\tmovq $0, %rax" << endl;
			cout << "\tcall printf@PLT" << endl;
			cout << "\tnop\t# Prevent problems with printf" << endl;
			break;

		case BOOLEAN:
			cout << "\tpop %rax \t# The value to be displayed" << endl;
			cout << "\tcmpq $0, %rax" << endl;
			cout << "\tje DisplayFalse" << displayNumber << "_" << ++localTag << endl;
			cout << "\tmovq $_StringTrue, %rdi" << endl;
			cout << "\tcall\tprintf@PLT" << endl;
			cout << "\tnop\t# Prevent problems with printf" << endl;
			cout << "\tjmp Display" << displayNumber << "_" << localTag << "End" << endl;
			cout << "DisplayFalse" << displayNumber << "_" << localTag << ":\t movq $_StringFalse, %rdi" << endl;
			cout << "\tcall\tprintf@PLT" << endl;
			cout << "\tnop\t# Prevent problems with printf" << endl;
			cout << "Display" << displayNumber << "_" << localTag << "End:" << endl;
			break;

		case CHAR: // CHAR is 1-byte long BUT stored in 8-byte in register (treatable as INTEGER)
			cout << "\tpop %rsi \t# The value to be displayed" << endl;
			cout << "\tmovq $_FormatStringInt, %rdi" << endl;
			cout << "\tmovq $_FormatStringChar, %rdi" << endl;
			cout << "\tmovq $0, %rax" << endl;
			cout << "\tcall printf@PLT" << endl;
			cout << "\tnop\t# Prevent problems with printf" << endl;
			break;

		case DOUBLE:
			cout << "\tpop %rsi \t# The value to be displayed" << endl;
			cout << "\tmovq $_FormatStringDouble, %rdi" << endl;
			break;

		default:
			cerr << "Type : " << charTypeToString(type);
			Error("Type inconnu pour le DISPLAY");
			cout << "\tmovq $0, %rax" << endl;
			cout << "\tcall printf@PLT" << endl;
			cout << "\tnop\t# Prevent problems with printf" << endl;
		}
	} while (current == COMMA);
	cout << "Display" << displayNumber << "End:\tmovq $_EmptyString, %rdi" << endl;
	cout << "\tcall puts@PLT" << endl; // Saut de ligne
}

// RepeatStatement := "REPEAT" Statement "UNTIL" Expression
void RepeatStatement(void)
{
	ReadKeyword("REPEAT");
	int localTag = ++TagNumber;
	cout << "_" << localTag  << "_Repeat:" << endl;
	Statement();
	ReadKeyword("UNTIL");
	TYPES type = Expression();
	if (type != BOOLEAN)
	{
		cerr << "Type de l'expression : " << charTypeToString(type) << endl;
		Error("L'expression dans le UNTIL doit être de type BOOLEAN");
	}
	cout << "_" << localTag  << "_RepeatTest:\t popq %rax" << endl;
	cout << "\tcmpq $0, %rax" << endl;
	cout << "\tje _" << localTag << "_Repeat" << endl;
	cout << "_" << localTag << "_RepeatEnd:" << endl;
}

// Keyword := "IF" | "THEN" | "ELSE" | "WHILE" | "DO" | "BEGIN" | "END"
// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
void Statement(void)
{
	if (current == KEYWORD)
	{
		if (ccmp("IF"))
		{
			IfStatement();
		}
		else if (ccmp("WHILE"))
		{
			WhileStatement();
		}
		else if (ccmp("FOR"))
		{
			ForStatement();
		}
		else if (ccmp("BEGIN"))
		{
			BlockStatement();
		}
		else if (ccmp("DISPLAY")) 
		{
			DisplayStatement();
		}
		else if (ccmp("REPEAT"))
		{
			RepeatStatement();
		}
		else if (ccmp("CASE"))
		{
			CaseStatement();
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
	cout << "\t.cfi_startproc\t# The main function must be visible from outside" << endl;
	cout << "main:\t\t\t# The main function body :" << endl;
	cout << "\tmovq %rsp, _stackTop\t# Save the position of the stack's top" << endl;
	do
	{
		ReadIf(SEMICOLON);
		Statement();
	} while (current == SEMICOLON);
	if (current != DOT)
		Error("caractère '.' attendu");
	Read();
}

// Program := {VarDeclarationPart} StatementPart
void Program(void)
{
	cout << "\t.data" << endl;
	cout << "\t.align 8" << endl;
	cout << "_FormatStringInt:\t .string \"%d \"" << endl;
	cout << "_FormatStringUInt:\t .string \"%llu \"" << endl;
	cout << "_FormatStringDouble:\t .string \"%lf \"" << endl;
	cout << "_FormatStringChar:\t .string \"%c\"" << endl;
	cout << "_StringTrue:\t .string \"TRUE \"" << endl;
	cout << "_StringFalse:\t .string \"FALSE \"" << endl;
	cout << "_EmptyString:\t .string \"\"" << endl;
	cout << "_stackTop:\t .quad 0" << endl;

	if (IsKeyword("VAR"))
		VarDeclarationPart();
	StatementPart();
}

int main(void)
{ // First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler enhanced by colin.palazzetti-rubera@alumni.univ-avignon.fr" << endl;
	Read();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq _stackTop, %rsp\t\t# Restore the position of the stack's top" << endl;
	cout << "\tnop" << endl;
	cout << "\tret\t\t\t# Return from main function" << endl;
	cout << "\t.cfi_endproc" << endl;
	if (current != FEOF)
	{
		cerr << "Caractères en trop à la fin du programme : [" << charTokenToString(current) << "]";
		Error("'.' déjà lu, fin du programme attendu"); // unexpected characters at the end of program
	}
}
