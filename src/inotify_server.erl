%%% File    : inotify_server.erl
%%% Author  : Defnull <define.null@gmail.com>
%%% Created : Среда, Апрель 11 2012 by Defnull
%%% Description : 

-module(inotify_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         add_watch/3,
         remove_watch/1,
         state/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("inotify.hrl").

-define(SERVER, ?MODULE). 
-define(PORT_TIMEOUT, 1000).
-define(INOTIFY_BIN, "inotify").

-record(watch, {filename,
                eventhandlers :: [inotify_handler()]
               }).

-record(state, {port,
                notify_instance,
                watches          %% key: WD | value: watch
               }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_watch(string(), [inotify_mask()], function()) -> ok | {error, any()}.
add_watch(Filename, Mask, Callback) ->
    gen_server:call(?MODULE, {add_watch, Filename, Mask, Callback}).

add_watch_impl(Filename, Mask, Callback, #state{port = Port,
                                                notify_instance = FD,
                                                watches = Watches} = State) ->
    try
        {ok, WD} = sync_call_command(Port, {add, FD, Filename, Mask}),
        Watches2 = dict:store(WD, #watch{filename = Filename, eventhandlers = Callback}, Watches),
        {reply, {ok, WD}, State#state{watches = Watches2}}
    catch _:Error ->
            {reply, {error, Error}, State}
    end.
    
remove_watch(Filename) ->
    gen_server:call(?MODULE, {remove_watch, Filename}).

remove_watch_impl(Filename, #state{port = Port, notify_instance = FD, watches = Watches} = State) ->
    try
        WD = search_for_watch(Filename, Watches),
        sync_call_command(Port, {remove, FD, WD}),
        Watches2 = dict:erase(WD, Watches),
        {reply, ok, State#state{watches = Watches2}}
    catch _:Error ->
            {reply, {error, Error}, State}
    end.

state() ->
    gen_server:call(?MODULE, state).

state(State) ->
    {reply, State, State}.

get_data({event, WD, Mask, Cookie, Name}, #state{watches = Watches} = State) ->
    Filename = search_for_filename(WD, Watches),
    get_data(WD, #inotify_event{filename = Filename,
                                mask = Mask,
                                cookie = Cookie,
                                name = Name},
             State).
get_data(WD, Event, #state{watches = Watches} = State) ->
    case dict:is_key(WD, Watches) of
        true ->
            #watch{filename = Filename, eventhandlers = EventHandler} = dict:fetch(WD, Watches),
            try apply(EventHandler, [Event])
            catch ErrorType:Error ->
                    log({callback_failed, Filename, ErrorType, Error})                        
            end;
        false ->
            log('bla-bla-bla')
    end,
    {noreply, State}.

search_for_watch(Filename, Watches) ->
    case [ WD || {WD, #watch{filename=Fname}} <- dict:to_list(Watches), Filename =:= Fname ] of
        [] ->
            undefined;
        [WD] ->
            WD
    end.

search_for_filename(WD, Watches) ->
    try
        Watch = dict:fetch(WD, Watches),
        Watch#watch.filename
    catch
        error:badarg ->
            undefined            
    end.

inotify_bin() ->       
     filename:join([code:priv_dir(inotify), ?INOTIFY_BIN]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Port = erlang:open_port({spawn, inotify_bin()}, [{packet, 2}, binary, exit_status]),
    {ok, FD} = sync_call_command(Port, {open}),
    {ok, #state{port = Port,
                notify_instance = FD,
                watches = dict:new()
               }}.

handle_call({add_watch, Filename, Mask, Callback}, _From, State) ->
    add_watch_impl(Filename, Mask, Callback, State);

handle_call({remove_watch, Filename}, _From, State) ->
    remove_watch_impl(Filename, State);

handle_call(state, _From, State) ->
    state(State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Msg}}, #state{port = Port} = State) ->
    get_data(binary_to_term(Msg), State);

handle_info({Port, {exit_status, Status}}, #state{port = Port}) ->
    exit({exit_status, Status});

handle_info(_Info, State) ->
    io:format("Info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{port = Port, notify_instance = FD } = _State) ->
    sync_call_command(Port, {close, FD}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log(Msg) ->
    error_logger:error_msg("~p~n", [Msg]).

sync_call_command(Port, Msg) ->
    try
        erlang:port_command(Port, term_to_binary(Msg)),
        receive 
            {Port, {data, Data}} -> 
                binary_to_term(Data)
        after ?PORT_TIMEOUT -> 
                throw(port_timeout)
    end
  catch 
    _:Error -> 
      throw({port_failed, {Error, Port, Msg}})
  end.

%% inotify_server:add_watch("/tmp", [all], fun(M) -> io:format("~p~n", [M]) end).
