.PHONY: compile clean dialyze test

REBAR=rebar3

compile :
	$(REBAR) compile

clean :
	$(REBAR) clean

dialyze :
	$(REBAR) dialyzer

test :
	$(REBAR) ct
