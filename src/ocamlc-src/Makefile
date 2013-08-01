
ROOT= ../..
OBUILD= $(ROOT)/_obuild

PKG_OCAMLC= ocamlc-src

OCAMLC_SRC= $(wildcard *.ml *.mli)
OCAMLC_JS= $(ROOT)/www/ocamlc.js
OCAMLC_BUILD= $(OBUILD)/$(PKG_OCAMLC)/$(PKG_OCAMLC)

all: $(OCAMLC_JS)

$(OCAMLC_JS): $(OCAMLC_BUILD).js
	cp $< $@

$(OCAMLC_BUILD).js: $(OCAMLC_MAIN)
	ocp-build $(PKG_OCAMLC)
	js_of_ocaml $(OCAMLC_BUILD).byte
