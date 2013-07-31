
include Makefile.config
include Makefile.rules

SOURCES= webworker.ml ace.ml global.ml myutils.ml myparser.ml request.ml \
	mycompile.ml filemanager.ml eventmanager.ml dialog.ml tabs.ml \
	sidepanel.ml centerpanel.ml login.ml errors_lexer.mll
SOURCESI= webworker.mli ace.mli global.mli myutils.mli conftypes.mli \
	myparser.mli request.mli mycompile.mli filemanager.mli \
	eventmanager.mli dialog.mli tabs.mli errors_format.mli

OCPDIR= ocp-indent-src
OCPLIB= -I $(OCPDIR) ocp_indent.cma

ACDIR= autocomplete
ACLIB= -I $(ACDIR) autocomplete.cma

REDIR= ~/.opam/4.00.1/lib/re
RELIB= -I $(REDIR) re.cma re_emacs.cma re_str.cma

OCAMLCDIR= ocamlc-src

TOPLVLDIR = toplevel-src
TOPLVLLIB = -I $(TOPLVLDIR)/toplevellib-4.00.1 \
	 ocamlcommon.cma ocamlbytecomp.cma ocamltoplevel.cma

SOURCES1= $(SOURCES:.mll=.ml)
OBJS= $(SOURCES1:.ml=.cmo)
OBJSI= $(SOURCESI:.mli=.cmi)

LIBS= $(OCPLIB) $(RELIB) $(ACLIB) $(TOPLVLLIB)

INCLUDEJS=-I . -I $(OCPDIR) -I $(ACDIR) -I $(REDIR) $(JSFLAGS) \
	-I $(TOPLVLDIR)/cmicomp -I $(TOPLVLDIR)  \
	-I $(OPAMLIB)/ocaml/compiler-libs \
	-I ~/.opam/4.00.1/lib/lwt \
	-I ~/.opam/4.00.1/lib/js_of_ocaml_compiler-libs \
	$(TOPLVLDIR)/toplevel_runtime.js

JSFLAGS = -pretty -linkall -toplevel $(INCLUDEJS_TOPLVL)

.PHONY: depend ocamlc.js

all: depend main.js
	cp main.js ../www/main.js

ocamlc.js:
	$(MAKE) -C $(OCAMLCDIR)
	cp $(OCAMLCDIR)/$@ ../www/

main.js: main.byte
	js_of_ocaml $(INCLUDEJS) $<


main.byte: ace.cmo global.cmo myutils.cmo completion_js.cmo $(OBJS) indent.cmo mytoplevel.cmo
	$(CAMLJS) $(LIBS) -o $@ $^ $*.ml


mytoplevel.cmo:
	$(CAMLJS) -c $(TOPLVLLIB) $*.ml

indent.cmo: indent.cmi
	$(MAKE) -C $(OCPDIR)
	$(CAMLJS) -c $(OCPLIB) $*.ml

completion_js.cmo: 
	$(MAKE) -C $(ACDIR)
	$(CAMLJS) -c $(ACLIB) $*.ml

$(OBJS): $(OBJSI) $(SOURCES1)
	$(CAMLJS) -c $*.ml

$(OBJSI): $(SOURCESI)
	$(CAMLJS) -c $*.mli

clean:
	rm -f *~ \#*\# *.cm[ioat] *.cmti *.annot
	rm -f *.byte a.out main.js .depend

clean-all: clean
	$(MAKE) -C $(OCAMLCDIR) clean
	$(MAKE) -C $(OCPDIR) clean
	$(MAKE) -C ./autocomplete clean


depend: $(SOURCES)
	$(CAMLDEP) -pp $(PP) *.mll *.mli *.ml > .depend

-include .depend

