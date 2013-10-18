MODULES=atm atm_db

.PHONY: all
all: $(patsubst %,ebin/%.beam,$(MODULES))

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc -o ebin $^

.PHONY: test
test: eunit
	@make dialyzer

.PHONY: eunit
eunit: $(patsubst %,ebin-test/%.beam,$(MODULES))
	@cd ebin-test && erl -noshell $(foreach module,$(patsubst ebin-test/%.beam,%,$^),-eval 'io:format("Testing $(module):~n").' -run $(module) test) -run init stop

.PHONY: dialyzer
dialyzer: .dialyzer_plt
	@echo "Running Dialyzer..."
	@dialyzer --plt .dialyzer_plt --src -r src -DTEST

.dialyzer_plt:
	@dialyzer --output_plt .dialyzer_plt --build_plt --apps erts kernel stdlib

ebin-test/%.beam: src/%.erl
	@mkdir -p ebin-test
	erlc -DTEST -o ebin-test $^

.PHONY: clean
clean:
	-rm ebin/*.beam ebin-test/*.beam

comma:= ,
empty:=
space:= $(empty) $(empty)

.PHONY: doc
doc: $(patsubst %,src/%.erl,$(MODULES))
	@mkdir -p doc
	erl -noshell -run edoc_run files '[$(subst $(space),$(comma),$(patsubst %,"%",$^))]' '[{dir,"doc"}]'
