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

# Grammar compilation / debugging
ANTLR4_JAR=antlr-4.1-complete.jar
ANTLR4_CLASSPATH=".:$(ANTLR4_JAR):$(CLASSPATH)" # Add ANTLRv4 to CLASSPATH
ANTLR4=java -jar $(ANTLR4_JAR)
GRUN=java org.antlr.v4.runtime.misc.TestRig

$(ANTLR4_JAR):
	curl -O http://www.antlr.org/download/$(ANTLR4_JAR)

compile-grammar: $(ANTLR4_JAR)
	$(ANTLR4) ICE.g4
	CLASSPATH=$(ANTLR4_CLASSPATH) javac ICE*.java
.PHONY: compile-grammar

debug-grammar: $(ANTLR4_JAR)
	CLASSPATH=$(ANTLR4_CLASSPATH) $(GRUN) ICE root -gui -encoding utf8
	# Insert string to be parsed, followed by newline and ^D, on standard input
.PHONY: debug-grammar

clean-grammar:
	rm -f *.tokens *.java *.class
.PHONY: clean-grammar

DIALYZER_PLT = .dialyzer_plt

dialyze: $(DIALYZER_PLT) compile
	dialyzer -r src deps/*/src --src --plt $<
.PHONY: dialyze

$(DIALYZER_PLT):
	dialyzer --build_plt --output_plt $@ --apps erts kernel stdlib mnesia

rebar:
	git clone git://github.com/rebar/rebar.git rebar.d
	cd rebar.d && ./bootstrap
	mv rebar.d/rebar $@
	rm -rf rebar.d/
