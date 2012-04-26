REBAR_BIN=rebar

compile :
	$(REBAR_BIN) compile

clean : 
	$(REBAR_BIN) clean

dialyze : .build_plt
	$(REBAR_BIN) dialyze | tee dialyze.log

.build_plt :
	$(REBAR_BIN) check-plt || $(REBAR_BIN) build-plt
