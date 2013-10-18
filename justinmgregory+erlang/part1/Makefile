MODULES=atm atm_db
REBAR=`which rebar || ./rebar`

.PHONY: all test clean deps compile eunit doc dialyzer

all: deps
	@make compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test: eunit
	@make dialyzer

eunit:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

doc: 
	@$(REBAR) doc

dialyzer: .dialyzer_plt
	@echo "Running Dialyzer..."
	@dialyzer --quiet --plt .dialyzer_plt --src -r src -DTEST -Wunmatched_returns -Werror_handling -Wrace_conditions -Wunderspecs -Wno_behaviours

.dialyzer_plt:
	@dialyzer --output_plt .dialyzer_plt --build_plt --apps erts kernel stdlib eunit

cover: .eunit/index.html
	open .eunit/index.html

.eunit/index.html:
	@make eunit
