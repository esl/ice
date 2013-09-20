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

DIALYZER_PLT = .dialyzer_plt

dialyze: $(DIALYZER_PLT) compile
	dialyzer -r src deps/tparser/src --src --plt $<
.PHONY: dialyzer

$(DIALYZER_PLT):
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib

rebar:
	git clone git://github.com/rebar/rebar.git rebar.d
	cd rebar.d && ./bootstrap
	mv rebar.d/rebar $@
	rm -rf rebar.d/

icy: test
	erl -pa .eunit -pa deps/*/ebin -s icy -eval 'io:format("Icy at http://localhost:8888\n").'
