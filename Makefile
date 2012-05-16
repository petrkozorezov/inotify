CT_RUN=ct_run
REBAR_BIN=./rebar
DIALYZER=dialyzer

compile :
	$(REBAR_BIN) get-deps
	$(REBAR_BIN) compile

clean : 
	$(REBAR_BIN) clean

dialyze : compile .build_plt
	$(DIALYZER) --plt inotify.plt -r ebin/

test : compile .specs
	$(CT_RUN) -noshell -pa $(PWD)/ebin -pa $(PWD)/deps/*/ebin -spec test/test.spec

.build_plt :
	$(DIALYZER) --check_plt --plt inotify.plt || $(DIALYZER) --add_to_plt -r ebin/ --output_plt inotify.plt

.specs: test/spec/test.spec.in
	sed -e "s,@PATH@,$(PWD)," test/spec/test.spec.in > $(PWD)/test/test.spec