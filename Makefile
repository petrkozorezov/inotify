REBAR_BIN=./rebar
DIALYZER=dialyzer

compile :
	$(REBAR_BIN) compile

clean : 
	$(REBAR_BIN) clean

dialyze : compile .build_plt
	$(DIALYZER) --plt inotify.plt -r ebin/

.build_plt :
	$(DIALYZER) --check_plt --plt inotify.plt || $(DIALYZER) --add_to_plt -r ebin/ --output_plt inotify.plt
