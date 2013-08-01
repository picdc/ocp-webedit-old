
ROOT= ..
OBUILD= $(ROOT)/_obuild

PKG_OCP-WEBEDIT= ocp-webedit-src

MAIN_SRC= $(wildcard *.ml *.mli)
MAIN_JS= $(ROOT)/www/main.js
MAIN_BUILD= $(OBUILD)/$(PKG_OCP-WEBEDIT)/$(PKG_OCP-WEBEDIT)

all: $(MAIN_JS)


$(MAIN_JS): $(MAIN_BUILD).js
	cp $< $@

$(MAIN_BUILD).js: $(MAIN_SRC)
	ocp-build $(PKG_OCP-WEBEDIT)
	js_of_ocaml $(MAIN_BUILD).byte

