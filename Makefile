.PHONY: all test clean compile run check
	
all:
	ERL_FLAGS=" -args_file ${PWD}/config/vm.args.src" rebar3 shell --apps miniclip
	
test:
	rebar3 ct --cover && rebar3 cover --verbose
	
clean:
	rebar3 clean
	
compile:
	rebar3 compile
	
check:
	rebar3 as test dialyzer
