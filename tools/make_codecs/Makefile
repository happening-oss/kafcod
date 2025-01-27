all: escriptize

escriptize:
	rebar3 escriptize
	ln -sf _build/default/bin/make_codecs .

eunit:
	rebar3 eunit

clean:
	rm -rf _build
