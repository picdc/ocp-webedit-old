
include Makefile.config
include Makefile.rules

SOURCES= webworker.ml ace.ml global.ml myutils.ml myparser.ml request.ml \
	mycompile.ml filemanager.ml errors_lexer.mll \
	eventmanager.ml errors_report.ml dialog.ml tabs.ml \
	sidepanel.ml centerpanel.ml mytoplevel.ml login.ml 
SOURCESI= webworker.mli ace.mli global.mli myutils.mli conftypes.mli \
	myparser.mli request.mli mycompile.mli filemanager.mli \
	errors_format.mli eventmanager.mli dialog.mli tabs.mli mytoplevel.mli

OCPDIR= ocp-indent-src
OCPLIB= -I $(OCPDIR) ocp_indent.cma

ACDIR= autocomplete
ACLIB= -I $(ACDIR) autocomplete.cma

REDIR= ~/.opam/4.00.1/lib/re
RELIB= -I $(REDIR) re.cma re_emacs.cma re_str.cma

OCAMLCDIR= ocamlc-src
TOPLVLDIR = toplevel-src

SOURCES1= $(SOURCES:.mll=.ml)
OBJS= $(SOURCES1:.ml=.cmo)
OBJSI= $(SOURCESI:.mli=.cmi)

LIBS= $(OCPLIB) $(RELIB) $(ACLIB)

INCLUDEJS=
# -I . -I $(OCPDIR) -I $(ACDIR) -I $(REDIR) $(JSFLAGS) \
# -I $(OPAMLIB)/ocaml/compiler-libs \
# -I ~/.opam/4.00.1/lib/lwt \
# -I ~/.opam/4.00.1/lib/js_of_ocaml_compiler-libs 

JSFLAGS = -pretty -linkall

.PHONY: depend ocamlc.js

all: depend main.js
	cp main.js ../www/main.js

ocamlc.js:
	$(MAKE) -C $(OCAMLCDIR)
	cp $(OCAMLCDIR)/$@ ../www/

toplevelw.js:
	$(MAKE) -C $(TOPLVLDIR)
	cp $(TOPLVLDIR)/$@ ../www/

main.js: main.byte
	js_of_ocaml $(JSFLAGS) $(INCLUDEJS) $<


main.byte: ace.cmo global.cmo myutils.cmo completion_js.cmo $(OBJS) indent.cmo mytoplevel.cmo
	$(CAMLJS) $(LIBS) -o $@ $^ $*.ml

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
	$(MAKE) -C $(TOPLVLDIR) clean
	$(MAKE) -C $(OCPDIR) clean
	$(MAKE) -C ./autocomplete clean


depend: $(SOURCES)
	$(CAMLDEP) -pp $(PP) *.mll *.mli *.ml > .depend

-include .depend

