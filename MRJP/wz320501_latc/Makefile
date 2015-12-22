all:
	bnfc -d -haskell src/Latte.cf
	happy -gca Latte/Par.y
	alex -g Latte/Lex.x
	ghc --make src/Latc_frontend.hs -o Latc_frontend -isrc
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
distclean: clean
	-rm -f Latte/Lex.* Latte/Par.* Latte/Layout.* Latte/Skel.* Latte/Print.*  Latte/Abs.* Latte/ErrM.* SharedString.* Latte.dtd XMLLatte.* Makefile*

