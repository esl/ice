all: compile
.PHONY: all

compile: rebar deps
	./rebar update-deps compile
.PHONY: compile

deps: rebar
	./rebar get-deps compile

clean: rebar
	./rebar clean
.PHONY: clean

distclean: clean
	rm -rf deps ebin .eunit rebar
.PHONY: distclean

debug: compile
	erl -pa ebin -pa deps/*/ebin
.PHONY: debug

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
	rm -rf rebar.d

isee: deps/isee compile
	cd deps/isee && ../../rebar get-deps compile
	erl -pa ebin/ -pa deps/*/ebin/ \
            -s isee \
            -eval 'io:format("Visualisor up at http://localhost:8888\n").'
.PHONY: isee

deps/isee: rebar
	git submodule init
	git submodule update
	mkdir -p deps/
	mv isee/ deps/isee/
