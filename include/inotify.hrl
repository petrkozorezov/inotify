%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2010 by mats cronqvist <masse@kreditor.se>

%% @doc
%% inotify masks
%% @end

-type inotify_mask_el() :: all
                         | access
                         | attrib
                         | close_write
                         | close_nowrite
                         | close
                         | create
                         | delete
                         | delete_self
                         | modify
                         | move_self
                         | moved_from
                         | moved_to
                         | move
                         | open
                         | dont_follow
                         | mask_add
                         | onlydir.

-type inotify_mask() :: inotify_mask_el() | [inotify_mask_el()].

-record(inotify_event, { filename ::       string(),
                         mask     :: inotify_mask(),
                         cookie   ::      integer(),
                         name     ::       string()
                       }).
-type inotify_event() :: #inotify_event{}.
-type inotify_handler() :: pid()
                         | fun((pid(), inotify_event()) -> any())
                         | {atom(), atom()}.

-type iwatch_id() :: reference().
-type inotify_id() :: {non_neg_integer(), non_neg_integer()}.

-record(watch, {handler, mon, inotify_id, filename, mask}).

-type iwatch() :: #watch{
    handler    :: inotify_handler(),
    mon        ::       reference(),
    inotify_id ::      inotify_id(),
    filename   ::          string(),
    mask       ::    inotify_mask()
}.

-record(state, {port, mq, table}).

-type istate() :: #state{
    port  ::        port(),
    mq    :: orddict:new(),
    table ::        atom()   %%  iwatch
}.
