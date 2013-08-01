
OBUILD= _obuild

PKG_OCP-WEBEDIT= ocp-webedit-src
PKG_OCAMLC= ocamlc-src

OCP-WEBEDIT= $(OBUILD)/$(PKG_OCP-WEBEDIT)/$(PKG_OCP-WEBEDIT)
OCAMLC= $(OBUILD)/$(PKG_OCAMLC)/$(PKG_OCAMLC)
TOPLEVEL= src/toplevel-src/toplevelw

all: server src

run:
	server/server -fg

dmaison-run:
	server/server -fg -conf server/notmy_server.conf

server:
	$(MAKE) -C server


src: $(OCP-WEBEDIT).byte $(OCAMLC).byte $(TOPLEVEL).byte



clean:
	ocp-build clean
	$(MAKE) -C src/toplevel-src clean
	$(MAKE) -C server clean
	rm -rf www/main.js www/ocamlc.js www/toplevel.js


$(OCP-WEBEDIT).byte: $(wildcard src/*.ml src/*.mli)
	ocp-build build $(PKG_OCP-WEBEDIT)
	js_of_ocaml $(OCP-WEBEDIT).byte
	cp $(OCP-WEBEDIT).js www/main.js

$(OCAMLC).byte: $(wildcard src/ocamlc-src/*.ml src/ocamlc-src/*.mli)
	ocp-build build $(PKG_OCAMLC)
	js_of_ocaml $(OCAMLC).byte
	cp $(OCAMLC).js www/ocamlc.js

$(TOPLEVEL).byte: $(wildcard src/toplevel-src/*.ml src/toplevel-src/*.mli)
	$(MAKE) -C src/toplevel-src
	cp $(TOPLEVEL).js www/toplevel.js
