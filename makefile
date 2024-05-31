all:	lancer
run:	total
total:	lancer
	./test/test
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
lancer:	machine
ddd:
		ddd ./test/test
gdb:
		gdb ./test/test
comp: clean compilateur
teste: clean test/teste.c
		gcc -g -c -S test/teste.c


testC: compilateur test/testAll.p
		./compilateur <test/testAll.p >test/testAll.s
testS: testC test/testAll.s
		gcc -ggdb -no-pie -fno-pie test/testAll.s -o test/testAll
test: testS
		./test/testAll



temp: test/test.s
		gcc -ggdb -no-pie -fno-pie test/test.s -o test/test
		./test/test