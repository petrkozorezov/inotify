%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2010 by mats cronqvist <masse@kreditor.se>

%% @doc
%% inotify masks
%% @end

-type inotify_mask() :: all 
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

-record(inotify_event, {filename :: string(),
                        mask :: [inotify_mask()],
                        cookie :: integer(),
                        name :: string()}).

-type inotify_event() :: #inotify_event{}.
-type inotify_handler() :: fun((inotify_event()) -> any()).                      

