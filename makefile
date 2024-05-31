all:	lancer
run:	total
total:	lancer
	./test/test
lancer:	machine
clean:
		rm -rf *.o *.s tokeniser.cpp
tokeniser: tokeniser.o
tokeniser.cpp:	tokeniser.l
		flex++ -d -otokeniser.cpp tokeniser.l
tokeniser.o:	tokeniser.cpp
		g++ -c -fPIE tokeniser.cpp
compilateur:	compilateur.cpp tokeniser.o
		g++ -ggdb -o compilateur compilateur.cpp tokeniser.o
compilation:		compilateur test/test.p
		./compilateur <test/test.p >test/test.s
machine: compilation test/test.s
		gcc -ggdb -no-pie -fno-pie test/test.s -o test/test
ddd:
		ddd ./test/test
gdb:
		gdb ./test/test

comp: clean compilateur


testC: compilateur test/testAll.p
		./compilateur <test/testAll.p >test/testAll.s
testS: testC test/testAll.s
		gcc -ggdb -no-pie -fno-pie test/testAll.s -o test/testAll
test: testS
		./test/testAll


test2C: compilateur test/test.p
		./compilateur <test/test.p >test/test.s
test2S: test2C test/test.s
		gcc -ggdb -no-pie -fno-pie test/test.s -o test/testAll
test2: test2S
		./test/testAll


testCode: clean codeTests/teste.c
		gcc -g -c -S codeTests/teste.c