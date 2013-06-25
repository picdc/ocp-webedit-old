include ../Makefile.config
include ../Makefile.rules


RELIB= -I ~/.opam/4.00.1/lib/re re.cma re_emacs.cma re_str.cma

INCLUDES= -package re -package re.str

SOURCES= completion_data.ml completion_lexer.mll

SOURCES1= $(SOURCES:.mll=.ml)
OBJS= $(SOURCES1:.ml=.cmo)

all: depend autocomplete.cmo

byte: $(OBJS) autocomplete.cmo
	$(CAMLFIND) $(CAMLC) -g $(INCLUDES) $^ \
	-linkpkg -o test_completion.byte test_completion.ml

depend: $(SOURCES)
	$(CAMLDEP) -pp $(PP) *.mli *.ml > .depend

autocomplete.cmo: $(OBJS)
	$(CAMLFIND) $(CAMLC) -g -package re $^ \
	-linkpkg -o autocomplete.cma -a $*.ml

clean:
	rm -rf *.cm[iaot] *.annot
	rm completion_lexer.ml


-include .depend
