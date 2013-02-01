NAME = ocaml-datalog

OCAMLMAKEFILE = OCamlMakefile

SOURCES = datalog.mli datalog.ml prover.ml parser.mly scanner.mll	\
reader.mli reader.ml main.ml

RESULT  = datalog

include $(OCAMLMAKEFILE)

test:
	./try

dist:
	DATE=`date --iso`; \
	find . -name .git -prune -o -print0 \
		| cpio -pmd0 ../$(NAME)-$${DATE}; \
	cd ..; \
	tar czf $(NAME)-$${DATE}.tar.gz $(NAME)-$${DATE}; \
	rm -rf $(NAME)-$${DATE}
