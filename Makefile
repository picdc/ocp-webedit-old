
OBUILD= _obuild

PKG_OCP-WEBEDIT= ocp-webedit-src
PKG_OCAMLC= ocamlc-src
PKG_TOPLEVEL= toplevel-src

DIR_TOPLEVEL= src/toplevel-src
DIR_OCAMLC= src/ocamlc-src

OCP-WEBEDIT= $(OBUILD)/$(PKG_OCP-WEBEDIT)/$(PKG_OCP-WEBEDIT)
OCAMLC= $(OBUILD)/$(PKG_OCAMLC)/$(PKG_OCAMLC)
TOPLEVEL= $(OBUILD)/$(PKG_TOPLEVEL)/$(PKG_TOPLEVEL)

JSFLAGS= -pretty -noinline
JSINCLUDES= -I $(DIR_TOPLEVEL)/cmicomp \
	    -I ~/.opam/4.00.1/lib/ocaml/compiler-libs \
	    -I ~/.opam/4.00.1/lib/js_of_ocaml_compiler-libs \

all: server src

run:
	server/server.byte -fg

dmaison-run:
	server/server.byte -fg -conf server/notmy_server.conf

server: server/server.byte

server/server.byte:
	$(MAKE) -C server


src: $(OCP-WEBEDIT).byte $(OCAMLC).byte $(TOPLEVEL).byte


clean: clean-src
	$(MAKE) -C server clean

clean-src:
	ocp-build clean
	rm -rf www/main.js www/ocamlc.js www/toplevel.js


$(OCP-WEBEDIT).byte: $(wildcard src/*.ml src/*.mli)
	ocp-build build $(PKG_OCP-WEBEDIT)
	js_of_ocaml $(JSFLAGS) $(OCP-WEBEDIT).byte
	cp $(OCP-WEBEDIT).js www/main.js

$(OCAMLC).byte: $(wildcard $(DIR_OCAMLC)/*.ml $(DIR_OCAMLC)/*.mli)
	ocp-build build $(PKG_OCAMLC)
	js_of_ocaml $(JSFLAGS) -toplevel $(JSINCLUDES) \
	   -I $(OBUILD)/$(PKG_TOPLEVEL) \
	   $(DIR_OCAMLC)/ocp-runtime.js \
	   $(DIR_OCAMLC)/stdlib_cma.js $(OCAMLC).byte
	cp $(OCAMLC).js www/ocamlc.js

$(TOPLEVEL).byte: $(wildcard $(DIR_TOPLEVEL)/*.ml $(DIR_TOPLEVEL)/*.mli)
	ocp-build build $(PKG_TOPLEVEL)
	js_of_ocaml $(JSFLAGS) -toplevel $(JSINCLUDES) \
	   -I $(OBUILD)/$(PKG_TOPLEVEL) \
	   $(DIR_TOPLEVEL)/toplevel_runtime.js \
	   $(DIR_TOPLEVEL)/stdout.js $(TOPLEVEL).byte
	cp $(TOPLEVEL).js www/toplevel.js
