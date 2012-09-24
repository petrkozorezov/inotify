%%% File    : inotify_watch_handler.erl
%%% Author  : Defnull <define.null@gmail.com>
%%% Created : Пятница, Май 11 2012 by Defnull

-module(inotify_watch_handler).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { server :: pid(),
                 handle :: function(),
                 monref
               }
       ).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

init(Options) ->
    Server = proplists:get_value(server, Options),
    {Callback, MonRef} = form_callback(proplists:get_value(callback, Options)),
    {ok, #state{ server = Server,
                 handle = Callback,
                 monref = MonRef
               }
    }.

form_callback({M,F}) when is_atom(M) and is_atom(F) ->
    {fun(Event) -> erlang:apply(M, F, [self(), Event]) end, undefined};
form_callback(Fun) when is_function(Fun) ->
    {fun(Event) -> Fun(self(), Event) end, undefined};
form_callback(Pid) when is_pid(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    {fun(Event) -> Pid ! {self(), Event} end, MonRef};
form_callback(undefined) ->
    erlang:error(callback_undefined).

handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({Server, Event}, #state{server = Server, handle = Handler} = State) ->
    Handler(Event),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, _, _}, #state{monref = MonRef} = State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

