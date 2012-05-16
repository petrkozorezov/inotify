%% File    : inotify_SUITE.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 16-05-2012 by Defnull
%% Description : 

-module(inotify_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    inotify:start(),
    ct:log("start"),
    Config.

end_per_suite(_Config) ->
    inotify:stop(),
    ct:log("end"),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [my_test_case].

my_test_case(Config) ->
    Val = process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(PrivDir, "does_not_exist"),
    
    {error, enoent} = inotify_server:add_watch(Name, [all], self()),

    process_flag(trap_exit, Val),
    ok.



