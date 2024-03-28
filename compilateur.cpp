//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//  Copyright (C) 2024 Colin PALAZZETTI RUBERA
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

using namespace std;

char current;				// Current char	

void ReadChar(void){		// Read character and skip spaces until 
				// non space character is read
	while(cin.get(current) && (current==' '||current=='\t'||current=='\n'))
	   	cin.get(current);
}

void Error(string s){
	cerr<< s << endl;
	exit(-1);
}


	
// AdditiveOperator := "+" | "-"
void AdditiveOperator(void){
	if(current=='+'||current=='-')
		ReadChar();
	else
		Error("Opérateur additif attendu");	   // Additive operator expected
}
		
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
void Digit(void){
	if((current<'0')||(current>'9'))
		Error("Chiffre attendu");		   // Digit expected
	else{
		cout << "\tpush $"<<current<<endl;
		ReadChar();
	}
}

void ArithmeticExpression(void);			// Called by Term() and calls Term()

// Term := Digit | "(" ArithmeticExpression ")"
void Term(void){
	if(current=='('){
		ReadChar();
		ArithmeticExpression();
		if(current!=')')
			Error("')' était attendu");		// ")" expected
		else
			ReadChar();
	}
	else 
		if (current>='0' && current <='9')
			Digit();
	     	else
			Error("'(' ou chiffre attendu");
}

// ArithmeticExpression := Term {AdditiveOperator Term}
void ArithmeticExpression(void){
	char adop;
	Term();
	while(current=='+'||current=='-'){
		adop=current;		// Save operator in local variable
		AdditiveOperator();
		Term();
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		if(adop=='+')
			cout << "\taddq	%rbx, %rax"<<endl;	// add both operands
		else
			cout << "\tsubq	%rbx, %rax"<<endl;	// substract both operands
		cout << "\tpush %rax"<<endl;			// store result
	}
}

char OpRel(void){	// Relational operator
	if(current!='='&&current!='<'&&current!='>'&&current!='!')
		Error("Opérateur relationnel attendu");
	char oprel=current;
	ReadChar();
	return oprel;
}
// ExpA == Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
void ExpA(void){
	if((current<'0')||(current>'9'))
		Error("Chiffre attendu");		   // Digit expected
	else{
		cout << "\tpush $"<<current<<endl;
		ReadChar();
	}
}

void Exp(void){
	ExpA();
	if(current=='<'||current=='>'||current=='='||current=='!'){
		char oprel = OpRel();
		ExpA();
		cout << "\tpop %rax"<<endl;
		cout << "\tpop %rbx"<<endl;
		cout << "\tcmpq %rax, %rbx"<<endl;
		switch (oprel)
		{
		case '<':
			cout << "\tjb Vrai\t# Aller sur code de vrai"<<endl;
			break;

		case '>':
			cout << "\tja Vrai\t# Aller sur code de vrai"<<endl;
			break;

		case '=':
			cout << "\tje Vrai\t# Aller sur code de vrai"<<endl;
			break;

		case '!':
			cout << "\tjne Vrai\t# Aller sur code de vrai"<<endl;
			break;
		
		default:
			Error("Opérateur relationnel attendu - default triggered");
			break;
		}
		cout << "Faux:\tpush $0\t# Faux sur la pile"<<endl;
		cout << "\tjmp FinExp\t# Aller à Fin, évider code faux"<<endl;
		cout << "Vrai:\tpush $-1\t# Code de vrai"<<endl;
		cout << "FinExp:";
	}
}

int main(void){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler"<<endl;
	cout << "\t.text\t\t# The following lines contain the program"<<endl;
	cout << "\t.globl main\t# The main function must be visible from outside"<<endl;
	cout << "main:\t\t\t# The main function body :"<<endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<endl;

	// Let's proceed to the analysis and code production
	ReadChar();
	// ArithmeticExpression();
	Exp();
	ReadChar();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<endl;
	cout << "\tret\t\t\t# Return from main function"<<endl;
	if(cin.get(current)){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}
		
			





