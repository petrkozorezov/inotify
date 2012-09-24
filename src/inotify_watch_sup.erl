%%% File    : inotify_watch_sup.erl
%%% Author  : Defnull <define.null@gmail.com>
%%% Created : Пятница, Май 11 2012 by Defnull

-module(inotify_watch_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = {one_for_one, 1000, 3600},
    {ok, {SupFlags, []}}.

