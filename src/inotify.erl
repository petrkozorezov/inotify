-module(inotify).
-export([start/0, start_link/0, stop/0 ]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    inotify_sup:start_link().

%% @spec start() -> ok
%% @doc Start the inotify server.
start() ->
    ensure_started(sasl),
    application:start(inotify).

%% @spec stop() -> ok
%% @doc Stop the inotify server.
stop() ->
    Res = application:stop(inotify),
    Res.

