%% File    : inotify_sup.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : Четверг, Апрель 26 2012 by Defnull
%% Description : 

-module(inotify_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Processes = [
                 ?CHILD(inotify_watch_sup, supervisor),
                 ?CHILD(inotify_server, worker)
                ],
    
    {ok, { {one_for_one, 10, 10}, Processes} }.
