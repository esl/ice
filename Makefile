all: compile
.PHONY: all

compile: rebar
	./rebar get-deps compile
.PHONY: compile

clean: rebar
	./rebar clean
.PHONY: clean

test: compile
	./rebar skip_deps=true eunit
.PHONY: test

rebar:
	git clone git://github.com/rebar/rebar.git rebar.d
	cd rebar.d && ./bootstrap
	mv rebar.d/rebar $@
	rm -rf rebar.d/
