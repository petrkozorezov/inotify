%%% File    : inotify_server.erl
%%% Author  : Defnull <define.null@gmail.com>
%%% Created : Среда, Апрель 11 2012 by Defnull
%%% Description : 

-module(inotify_server).
-behaviour(gen_server).
-compile({parse_transform, sheriff}).

%% API
-export([start_link/0, add_watch/3, add_watch_link/3, remove_watch/1 ]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("inotify.hrl").

-define(SERVER, ?MODULE). 
-define(PORT_TIMEOUT, 1000).
-define(INOTIFY_BIN, "inotify").
-define(INOTIFY_ETS, inotify_table).
-define(SHUTDOWN_TIMEOUT, 2000).

-export_type([inotify_event/0,
              inotify_handler/0]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add inotify watch on the file or dir. If file is already whatched with inotify instance creates new
%% Callback can be fun, tuple of atoms {Module,Function} or a pid(). 
-spec add_watch(string(), inotify_mask(), inotify_handler() | {atom(), atom()} | pid()) ->
                       {ok, pid()} | {error, any()}.
add_watch(Filename, Mask, Callback) ->
    case sheriff:check(Mask, inotify_mask) of
        true ->
            gen_server:call(?MODULE, {add_watch, Filename, Mask, Callback});
        false ->
            {error, badmask}
    end.     

%% @doc Similiar to add_watch but adds linking with watching process.
-spec add_watch_link(string(), inotify_mask(), inotify_handler() | {atom(), atom()} | pid()) ->
                       {ok, pid()} | {error, any()}.
add_watch_link(Filename, Mask, Callback) ->
    case add_watch(Filename, Mask, Callback) of
        {ok, Pid} ->
            link(Pid),
            {ok, Pid};
        {error, Any} ->
            {error, Any}
    end.

-spec add_watch_impl(string(), inotify_mask(), inotify_handler(), istate()) -> {reply, any(), istate()}. 
add_watch_impl(Filename, Mask, Callback, #state{port=Port, mq=MQ} = State) ->
    try
        {MQ1, FD} = handle_free_instance(Filename, MQ, Port),
        case add_to_watch_instance(Port, FD, Filename, Mask) of
            {ok, WD} ->
                Child = add_handler({FD,WD}, Callback),
                
                Record = #watch{handler = Child,
                                mon = erlang:monitor(process, Child),
                                inotify_id = {FD,WD},
                                filename = Filename,
                                mask = Mask
                               },
                true = ets:insert(?INOTIFY_ETS, Record),
                MQ2 = orddict:update_counter(FD, 1, MQ1),
                {reply, {ok, Child}, State#state{mq = MQ2}};
            {error, Any} ->
                {reply, {error, Any}, State#state{mq = MQ1}}
        end
    catch _:Error ->
            log(Error),
            {reply, {error, Error}, State}
    end.

add_handler(Id, Callback) ->
    Options = [{callback, Callback}, {server, self()}],
    ChildSpec = { Id, {inotify_watch_handler, start_link, [Options]},
                  temporary, ?SHUTDOWN_TIMEOUT, worker, [inotify_watch_handler]
                },
    {ok, Pid} = supervisor:start_child(inotify_watch_sup, ChildSpec),
    Pid.

handle_free_instance(Filename, MQ, Port) ->
    Matched = ets:match_object(?INOTIFY_ETS, #watch{filename=Filename, _='_'}),
    Expect = [FD || #watch{inotify_id = {FD, _}} <- Matched],
    case most_unloaded_instance(MQ, Expect) of
        undefined ->
            case open_instance(Port) of
                {ok, FD} ->
                    {orddict:update_counter(FD, 0, MQ), FD};
                {error, Any} ->
                    error({error, {<<"Attempt to create inotify instance failed">>, Any}})
            end;
        FD ->
            {MQ, FD}
    end.

most_unloaded_instance(MQ, Expect) ->
    Filtered = lists:filter(fun({A,_}) ->
                                    not lists:member(A, Expect)
                            end, MQ),
    most_unloaded_instance(Filtered).
most_unloaded_instance(MQ) ->
    SortFun = fun({A1,A2},{B1,B2}) -> {A2, A1} =< {B2, B1} end,
    case lists:sort(SortFun, MQ) of
        [] ->
            undefined;
        [{FD,_}|_] ->
            FD
    end.

%% @doc Removes wathing of the file or dir.
-spec remove_watch(pid()) -> ok.
remove_watch(HandlerPid)
  when is_pid(HandlerPid) ->
    unlink(HandlerPid),
    gen_server:call(?MODULE, {remove_watch, HandlerPid}).

remove_watch_impl(HandlerPid, State)
  when is_pid(HandlerPid) ->
    case ets:lookup(?INOTIFY_ETS, HandlerPid) of
        [] ->
            {error, no_watch};
        [Record] ->
            ets:delete(?INOTIFY_ETS, HandlerPid),
            {ok, NewState} = remove_watch_impl(Record, State),
            {reply, ok, NewState}
    end;

remove_watch_impl(#watch{inotify_id = {FD,_} = ID}, #state{port=Port, mq=MQ} = State) ->
    remove_from_tree(ID),
    {ok, _} = rm_from_watch_instance(Port, ID),
    MQ1 = orddict:update_counter(FD, -1, MQ),
    MQ2 = check_for_useless_instances(Port, MQ1),
    {ok, State#state{mq = MQ2}}.

check_for_useless_instances(Port, MQ1) ->
    EmptyInstances = orddict:filter(fun(_, N) when N =< 0 ->
                                            true;
                                       (_,_) -> false
                                    end, MQ1),
    [close_instance(Port, Fd) || Fd <- orddict:fetch_keys(EmptyInstances)],
    orddict:filter(fun(Fd, _) ->
                           not orddict:is_key(Fd, EmptyInstances)
                   end, EmptyInstances).

remove_from_tree(Id) ->
    Supervisor = inotify_watch_sup,
    supervisor:terminate_child(Supervisor, Id),
    supervisor:delete_child(Supervisor, Id).

get_data({event,FD, WD, Mask, Cookie, Name}, State) ->
    %% Ищем процесс обслуживающий эту подписку и отправляем ему сообщение
    case ets:match_object(?INOTIFY_ETS, #watch{inotify_id = {FD,WD}, _='_'}) of
        [] ->
            ok;
        [Record] ->
            Event = #inotify_event{filename=Record#watch.filename, mask=Mask, cookie=Cookie, name=Name},
            Handler = search_child(inotify_watch_sup, {FD,WD}),
            gen_server:cast(Handler, {self(), Event})
    end,
    {noreply, State}.

search_child(Supervisor, ChildId) ->
    case lists:keysearch(ChildId, 1, supervisor:which_children(Supervisor)) of
        {value, {ChildId, Child, _, _}} ->
            Child;
        false ->
            exit({no_child, ChildId, Supervisor})
    end.

inotify_bin() ->       
     filename:join([code:priv_dir(inotify), ?INOTIFY_BIN]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Port = erlang:open_port({spawn, inotify_bin()}, [{packet, 2}, binary, exit_status]),
    Table = ets:new(?INOTIFY_ETS, [named_table, set, protected, {keypos,2}]),

    {ok, #state{port=Port, mq=[], table=Table}}.

handle_call({add_watch, Filename, Mask, Callback}, _From, State) ->
    add_watch_impl(Filename, Mask, Callback, State);

handle_call({remove_watch, Filename}, _From, State) ->
    remove_watch_impl(Filename, State);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Msg}}, #state{port = Port} = State) ->
    get_data(binary_to_term(Msg), State);

handle_info({Port, {exit_status, Status}}, #state{port = Port}) ->
    %%TODO Do we need to restart port here?
    log({exit_status, Status}),
    exit({exit_status, Status});

handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, State) ->
    case ets:match_object(?INOTIFY_ETS, #watch{mon=MonitorRef, _='_'}) of
        [] ->
            {noreply, State};
        [Record] ->
            ets:delete(?INOTIFY_ETS, Record#watch.handler),
            {ok, NewState} = remove_watch_impl(Record, State),
            {noreply, NewState}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port=Port, mq=MQ} = _State) ->
    [remove_from_tree(ID) || ID <- ets:match(?INOTIFY_ETS,#watch{handler='$1', _='_'})],
    ets:delete(?INOTIFY_ETS),
    [close_instance(Port, FD) || {FD, _} <- MQ],
    erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

open_instance(Port) ->
    sync_call_command(Port, {open}).
close_instance(Port, FD) when is_integer(FD) ->
    sync_call_command(Port, {close, FD});
close_instance(Port, {FD,WD}) when is_integer(FD) and is_integer(WD) ->
    sync_call_command(Port, {close, FD}).
add_to_watch_instance(Port, FD, Filename, Mask) ->
    sync_call_command(Port, {add, FD, Filename, Mask}).
rm_from_watch_instance(Port, {FD,WD})
  when is_integer(FD) and is_integer(WD) ->
    sync_call_command(Port, {remove, FD, WD}).

%%-------------------------------------------------------------------

sync_call_command(Port, Msg) ->
    try
        true = erlang:port_command(Port, term_to_binary(Msg)),
        receive
            %% Tuple, which contains only two elements signals that it is command request
            %% http://www.erlang.org/doc/apps/erts/erl_ext_dist.html  VERSION(131) + SMALL_TUPLE_EXT(104,2)
            {Port, {data, Data = <<131,104,2,_/binary>>}} -> 
                binary_to_term(Data)
        after ?PORT_TIMEOUT -> 
                throw(port_timeout)
        end
    catch 
        _:Error -> 
            throw({port_failed, {Error, Port, Msg}})
    end.

log(Msg) ->
    error_logger:error_msg("~p~nStacktrace:~p~n", [Msg, erlang:get_stacktrace()]).
