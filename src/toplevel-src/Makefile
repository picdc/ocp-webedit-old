
MAIN= toplevelw.ml
SRC= $(MAIN)
SRCI= $(MAIN:.ml=.mli)
BYTE= $(MAIN:.ml=.byte)
JS= $(MAIN:.ml=.js)

CMIS=$(SRCI:.mli=.cmi)
CMOS=$(SRC:.ml=.cmo)

COMPLIBS= ~/.opam/4.00.1/lib/ocaml/compiler-libs
JSCOMPLIB= ~/.opam/4.00.1/lib/js_of_ocaml_compiler-libs

INCLUDES= -I $(COMPLIBS) ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma
INCLUDESJS= -I $(COMPLIBS) -I cmicomp -I . -I $(JSCOMPLIB) \
	 toplevel_runtime.js stdout.js


JSFLAGS= -pretty -noinline -toplevel
JSPKGS= -package js_of_ocaml,js_of_ocaml.syntax,js_of_ocaml_compiler,js_of_ocaml_compiler-libs \
	 -syntax camlp4o -linkpkg

PP = "camlp4o ~/.opam/4.00.1/lib/js_of_ocaml/pa_js.cmo"

CAMLC=ocamlc
CAMLFIND=ocamlfind
CAMLDEP=ocamldep -pp $(PP)
CAMLJS= $(CAMLFIND) $(CAMLC) $(JSPKGS) $(FLAGS) 


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLJS) $(INCLUDES) -c $<

.mli.cmi:
	$(CAMLJS) $(INCLUDES) -c $<

all: depend $(CMIS) $(BYTE)
	js_of_ocaml $(JSFLAGS) $(INCLUDESJS) $(BYTE)

$(BYTE): $(CMOS)
	$(CAMLJS) $(INCLUDES) $^ -o $@

depend: $(SRC)
	$(CAMLDEP) *.mli *.ml > .depend

clean:
	rm -rf *.cm[ioaxt] $(JS) $(BYTE)
