PROJECT := api_versions

all: kafcod
	rebar3 escriptize
	ln -sf _build/default/bin/$(PROJECT) .

kafcod:
	mkdir -p _checkouts
	[ -L _checkouts/kafcod ] || ln -s ../../.. _checkouts/kafcod
