
SRC_I = main.mli compile.mli errors.mli main_args.mli pparse.mli
SRC= compile.ml errors.ml main_args.ml pparse.ml main.ml

CMIS=$(SRC_I:.mli=.cmi)
CMOS=$(SRC:.ml=.cmo)

INC= ocamlcommon.cma ocamlbytecomp.cma 
INCLUDES= -I ~/.opam/4.00.1/lib/ocaml/compiler-libs $(INC)

CAMLC=ocamlfind ocamlc
CAMLDEP=ocamldep


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) $(INCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) -c $<


all: depend $(CMIS) ocamlc
	js_of_ocaml ocamlc.byte


ocamlc: $(CMOS)
	$(CAMLC) $(INCLUDES) $^ -o $@.byte 

depend: $(SRC) $(SRC_I)
	$(CAMLDEP) *.mli *.ml > .depend


clean:
	rm -rf *.o *.byte *.cm[ioax] ocamlc.js

-include .depend
