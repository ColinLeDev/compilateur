# CERIcompiler

A simple compiler.
From : Pascal-like imperative LL(k) langage
To : 64 bit 80x86 assembly langage (AT&T)

**Download the repository :**

> git clone git@framagit.org:jourlin/cericompiler.git

**Build the compiler and test it :**

> make test

**Have a look at the output :**

> gedit test.s

**Debug the executable :**

> ddd ./test

**Commit the new version :**

> git commit -a -m "What's new..."

**Send to your framagit :**

> git push -u origin master

**Get from your framagit :**

> git pull -u origin master

**This version Can handle :**

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Identifier {"," Identifier} "]"
// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement
// AssignementStatement := Identifier ":=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}
// Identifier := Letter {(Letter|Digit)}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"


## Ajouts personnels :
// RepeatStatement := "REPEAT" Statement "UNTIL" Expression

// DisplayStatement != "DISPLAY" (!|Expression) {"," (!|Expression)}
> Chaque valeur est affichée à la suite, sséparées par des espaces (SAUF POUR DES CHAR), puis un passage à la ligne est effectué
> DISPLAY !. affiche un premier passage à la ligne (! = '\n'), puis un deuxième (celui du DISPLAY)

> ```DISPLAY !.```
```./test/test

 
```

> ```DISPLAY !, 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!'.```
```./test/test

Hello World!
 
```

TOUS les tags utilisés dans le compilateur pour autre chose que les variables commencent (Devraient commencer pour une part) par un _, pour laisser toute latitude aux identifiants
> On aurait pu simplement ajouter `_` à la fin, ou simplement faire en sorte que les variables soient de la forme `var_{ID}`


## Limites détectées :
FOR : Si erreur "Mot clé `DOWNTO` attendu", le mot clé `TO` fait aussi l'affaire (gestion simplifiée de l'attente)

Le traitement des strings n'est pas permis, puisque en regardant comment le C effectue cette gestion, les strings sont pré-mises dans une variable mémoire : (voir codeTests)

`g++ -S -o codeTests/coutStr.s codeTests/coutStr.cpp`
```
	.section	.rodata
.LC0:
	.string	"Hello World!"
.LC1:
	.string	"Hello World2!"
	.text
	.globl	main
	.type	main, @function
```

# Tester :
> `make test` → tester avec test/testAll.p
> `make test2` → tester avec test/test.p
