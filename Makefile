all: compile
.PHONY: all

compile: rebar
	./rebar get-deps compile
.PHONY: compile

clean: rebar
	./rebar clean
.PHONY: clean

debug: compile
	erl -pa ebin -pa deps/*/ebin -env ERL_MAX_ETS_TABLES 256000
.PHONY: debug

test: compile
	./rebar skip_deps=true eunit
.PHONY: test

# Grammar compilation / debugging (Add ANTLRv4 to CLASSPATH)
compile-grammar:
	antlr4 ICE.g4
	javac ICE*.java
.PHONY: compile-grammar

debug-grammar:
	java org.antlr.v4.runtime.misc.TestRig ICE root -gui -encoding utf8
.PHONY: debug-grammar

clean-grammar:
	rm -f *.tokens *.java *.class
.PHONY: clean-grammar

DIALYZER_PLT = .dialyzer_plt

dialyze: $(DIALYZER_PLT) compile
	dialyzer -r src deps/*/src --src --plt $<
.PHONY: dialyzer

$(DIALYZER_PLT):
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib mnesia

rebar:
	git clone git://github.com/rebar/rebar.git rebar.d
	cd rebar.d && ./bootstrap
	mv rebar.d/rebar $@
	rm -rf rebar.d/
