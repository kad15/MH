# Makefile pas optimisé mais qui nous a suffi

.PHONY: src doc clean cleandoc distclean

EXE = mh
SRC = src
DOC = doc

C = ocamlfind c -thread -I $(SRC) -unsafe -package
OPT = ocamlfind opt -thread -I $(SRC) -unsafe -package

src:
	$(C)   zarith                $(SRC)/zarith_1_4.mli
	$(C)   bignum                $(SRC)/math.mli
	$(C)   bignum                $(SRC)/LLL.mli
	$(C)   batteries             $(SRC)/crypto.mli
	$(C)   batteries,cmdliner    $(SRC)/mh.mli
	$(OPT) bignum -c -w -58      $(SRC)/math.ml
	$(OPT) bignum -c             $(SRC)/LLL.ml
	$(OPT) batteries -c          $(SRC)/crypto.ml
	$(OPT) batteries,cmdliner -c $(SRC)/mh.ml
	$(OPT) batteries,bignum,cmdliner -linkpkg -o $(EXE) $(SRC)/math.cmx $(SRC)/LLL.cmx $(SRC)/crypto.cmx $(SRC)/mh.cmx

doc:
	mkdir -p $(DOC)
	ocamlfind doc -package bignum,batteries -thread -I $(SRC) -hide Pervasives -hide-warnings -html -colorize-code -d $(DOC) $(SRC)/*.mli

clean:
	rm -f $(SRC)/*.cm[iox] $(SRC)/*.o
	rm -f $(EXE)

cleandoc:
	rm -rf $(DOC)

distclean: clean cleandoc
