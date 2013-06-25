
include ../Makefile.config
include ../Makefile.rules

SOURCES = ocpUtil.ml pos.ml compat.ml approx_tokens.ml approx_lexer.mll \
	  nstream.ml indentBlock.ml indentPrinter.ml indentConfig.ml

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
OBJS = $(SOURCES2:.ml=.cmo)

all: depend ocp_indent.cma

.SUFFIXES:
.SUFFIXES: .ml .mli .mll .cmo .cmi


ocp_indent.cma: $(OBJS)
	$(CAMLC) -a -o $@ $(FLAGS) $^

clean:
	rm -f *~ \#*\# *.cm[ioa] *.annot

depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

include .depend