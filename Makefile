
include Makefile.config

PKG_MAIN= main-src
PKG_OCAMLC= ocamlc-src
PKG_TOPLEVEL= toplevel-src

DIR_TOPLEVEL= src/toplevel-src
DIR_OCAMLC= src/ocamlc-src
DIR_MAIN= src/main-src

MAIN= _obuild/$(PKG_MAIN)/$(PKG_MAIN)
OCAMLC= _obuild/$(PKG_OCAMLC)/$(PKG_OCAMLC)
TOPLEVEL= _obuild/$(PKG_TOPLEVEL)/$(PKG_TOPLEVEL)

SERVER= server/server.asm

JSFLAGS= -pretty -noinline
JSINCLUDES= -I $(DIR_TOPLEVEL)/cmicomp -I $(COMPILER-LIBS) -I $(JS_COMPILER-LIBS)

all: $(SERVER) src

run:
	killall $(SERVER)
	$(SERVER)


$(SERVER): $(wildcard server/*.ml)
	$(MAKE) -C server


src: $(MAIN).byte $(OCAMLC).byte $(TOPLEVEL).byte www/index.html


clean: clean-src
	$(MAKE) -C server clean

clean-src:
	ocp-build clean
	rm -rf www/main.js www/ocamlc.js www/toplevel.js


www/index.html: src/index.html
	cp $< $@



$(MAIN).byte: $(wildcard $(DIR_MAIN)/*.ml $(DIR_MAIN)/*.mli)
	ocp-build build $(PKG_MAIN)
	js_of_ocaml $(JSFLAGS) $(MAIN).byte
	cp $(MAIN).js www/main.js

$(OCAMLC).byte: $(wildcard $(DIR_OCAMLC)/*.ml $(DIR_OCAMLC)/*.mli)
	ocp-build build $(PKG_OCAMLC)
	js_of_ocaml $(JSFLAGS) -toplevel $(JSINCLUDES) \
	   -I _obuild/$(PKG_TOPLEVEL) \
	   $(DIR_OCAMLC)/ocp-runtime.js \
	   $(DIR_OCAMLC)/stdlib_cma.js $(OCAMLC).byte
	cp $(OCAMLC).js www/ocamlc.js

$(TOPLEVEL).byte: $(wildcard $(DIR_TOPLEVEL)/*.ml $(DIR_TOPLEVEL)/*.mli)
	ocp-build build $(PKG_TOPLEVEL)
	js_of_ocaml $(JSFLAGS) -toplevel $(JSINCLUDES) \
	   -I _obuild/$(PKG_TOPLEVEL) \
	   $(DIR_TOPLEVEL)/toplevel_runtime.js \
	   $(DIR_TOPLEVEL)/stdout.js $(TOPLEVEL).byte
	cp $(TOPLEVEL).js www/toplevel.js
