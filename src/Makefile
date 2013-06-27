
include Makefile.config
include Makefile.rules

SOURCES= ace.ml ace_utils.ml request.ml filemanager.ml event_manager.ml \
	dialog.ml tabs.ml sidepanel.ml 
SOURCESI= ace.mli ace_utils.mli request.mli filemanager.mli \
	event_manager.mli dialog.mli tabs.mli

OCPDIR= ocp-indent-src
OCPLIB= -I $(OCPDIR) ocp_indent.cma

ACDIR= autocomplete
ACLIB= -I $(ACDIR) autocomplete.cma

REDIR= ~/.opam/4.00.1/lib/re
RELIB= -I $(REDIR) re.cma re_emacs.cma re_str.cma

TOPLVLDIR = toplevel-src
TOPLVLLIB = -I $(TOPLVLDIR)/toplevellib-4.00.1 \
	 ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma


#TOPLVLDIR= tryocaml-without_js
#TOPLVLDIRLIB= $(TOPLVLDIR)/toplevellib/toplevellib-4.00.1
#TOPLVLLIB= -package js_of_ocaml_compiler \
#	-I $(TOPLVLDIR)/ocaml-num ocaml-num.cma \
#	-I $(TOPLVLDIRLIB) ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.#cma \
#	-I $(TOPLVLDIR)/try-ocaml lessons.cmo \
#	-I $(TOPLVLDIR)/tutorial tutorial.cma \
#	-I $(TOPLVLDIR)/ocp-jslib ocp-jslib.cma \
#	-I $(TOPLVLDIR)/toplevel toplevel.cma 

OBJS= $(SOURCES:.ml=.cmo)
OBJSI= $(SOURCESI:.mli=.cmi)

LIBS= $(OCPLIB) $(RELIB) $(ACLIB) $(TOPLVLLIB)

INCLUDEJS=-I . -I $(OCPDIR) -I $(ACDIR) -I $(REDIR) $(JSFLAGS) \
	-I $(TOPLVLDIR)/cmicomp -I $(TOPLVLDIR)  \
	-I $(OPAMLIB)/ocaml/compiler-libs \
	-I ~/.opam/4.00.1/lib/lwt \
	-I ~/.opam/4.00.1/lib/js_of_ocaml_compiler-libs \
	$(TOPLVLDIR)/toplevel_runtime.js

JSFLAGS = -pretty -noinline -linkall -toplevel $(INCLUDEJS_TOPLVL)

.PHONY: depend

all: depend main.js
	cp main.js ../www/main.js

main.js: main.byte
	js_of_ocaml $(INCLUDEJS) $<


main.byte: ace.cmo ace_utils.cmo completion_js.cmo $(OBJS) indent.cmo mytoplevel.cmo
	$(CAMLJS) $(LIBS) -o $@ $^ $*.ml


mytoplevel.cmo:
	$(CAMLJS) -c $(TOPLVLLIB) $*.ml

indent.cmo: 
	$(MAKE) -C $(OCPDIR)
	$(CAMLJS) -c $(OCPLIB) $*.ml

completion_js.cmo: 
	$(MAKE) -C $(ACDIR)
	$(CAMLJS) -c $(ACLIB) $*.ml

$(OBJS): $(OBJSI) $(SOURCES)
	$(CAMLJS) -c $*.ml

$(OBJSI): $(SOURCESI)
	$(CAMLJS) -c $*.mli

clean:
	rm -f *~ \#*\# *.cm[ioat] *.cmti *.annot
	rm -f *.byte a.out main.js .depend

clean-all: clean
	$(MAKE) -C $(OCPDIR) clean
	$(MAKE) -C ./autocomplete clean


depend: $(SOURCES)
	$(CAMLDEP) -pp $(PP) *.mli *.ml > .depend

-include .depend



test:
	ocamlc -I toplevellib-4.00.1 ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma -o test.byte test.ml
