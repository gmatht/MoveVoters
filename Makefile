# $Id: Makefile 31 2005-08-31 19:27:52Z smimram $
SOURCES = movevoters.ml
RESULT = movevoters
LIBS = glpk str
INCDIRS = $(shell ocamlfind query glpk)

-include OCamlMakefile

frap2: frap2.c
	gcc -O3 frap2.c -o frap2

ab_parsed.txt: ab.txt frap2 parse_ab.pl
	perl parse_ab.pl < ab.txt | grep -v ^[23] | ./frap2 1000 > ab_parsed.txt || true

roundvoters: roundvoters.ml
	ocamlc -g -I $(INCDIRS)         glpk.cma str.cma   -o roundvoters roundvoters.ml 

www: roundvoters movevoters
	cp -p roundvoters /usr/lib/cgi-bin/roundvoters.cgi
	cp -p movevoters  /usr/lib/cgi-bin/movevoters.cgi
	cp -p roundvoters.html movevoters.html /var/www
