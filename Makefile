

all: src server


src:
	$(MAKE) -C src

server:
	$(MAKE) -C server

run:
	server/server -fg

myrun:
	server/server -fg -conf server/notmy_server.conf


clean:
	$(MAKE) -C src clean-all
#	$(MAKE) -C server clean
	rm -rf www/main.js


.PHONY: all src server run clean
