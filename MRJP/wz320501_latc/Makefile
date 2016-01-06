all:
	gcc -S lib/runtime.c -o lib/runtime.s
	gcc -g -c lib/runtime.s -o lib/runtime.o
	bnfc -d -haskell src/Latte.cf
	happy -gca Latte/Par.y
	alex -g Latte/Lex.x
	ghc --make src/Latc_compiler.hs -o Latc_compiler -isrc
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
distclean: clean
	-rm -f Latte/Lex.* Latte/Par.* Latte/Layout.* Latte/Skel.* Latte/Print.*  Latte/Abs.* Latte/ErrM.* SharedString.* Latte.dtd XMLLatte.* Makefile*

