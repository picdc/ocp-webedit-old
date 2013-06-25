

all: src server


src:
	$(MAKE) -C src

server:
	$(MAKE) -C server

run:
	server/server -fg


clean:
	$(MAKE) -C src clean-all
#	$(MAKE) -C server clean
	rm -rf www/main.js


.PHONY: all src server run clean