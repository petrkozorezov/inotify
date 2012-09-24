%% File    : inotify_SUITE.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 16-05-2012 by Defnull
%% Description : 

-module(inotify_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("inotify/include/inotify.hrl").

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
    [does_not_exist,
     create_file,
     delete_file,
     delete_file2
    ].

does_not_exist(Config) ->
    Val = process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    Name = filename:join(PrivDir, "does_not_exist"),
    
    {error, enoent} = inotify_server:add_watch(Name, [all], self()),

    process_flag(trap_exit, Val),
    ok.

create_file(Config) ->
    Val = process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    File = "created.file",
    FullPath = filename:join(PrivDir, File),
    
    {ok, Pid} = inotify_server:add_watch(PrivDir, [create], self()),
    ok = file:write_file(FullPath, <<"ok">>),
    receive
        {Pid, #inotify_event{name=File, mask=[create]}} ->
            ct:log("ok")
    after
        100 ->
            print_received(),
            throw(timeout)
    end,
    
    process_flag(trap_exit, Val),
    ok.

delete_file(Config) ->
    Val = process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    File = "created.file",
    FullPath = filename:join(PrivDir, File),

    ok = file:write_file(FullPath, <<"ok">>),
    {ok, Pid} = inotify_server:add_watch(PrivDir, [delete], self()),
    ok = file:delete(FullPath),
    receive
        {Pid, #inotify_event{name=File, mask=[delete]}} ->
            ct:log("ok")
    after
        100 ->
            print_received(),
            throw(timeout)
    end,
    
    process_flag(trap_exit, Val),
    ok.

delete_file2(Config) ->
    Val = process_flag(trap_exit, true),
    PrivDir = proplists:get_value(priv_dir, Config),
    File = "created.file",
    FullPath = filename:join(PrivDir, File),

    ok = file:write_file(FullPath, <<"ok">>),
    {ok, Pid} = inotify_server:add_watch(FullPath, [delete_self], self()),
    ok = file:delete(FullPath),
    receive
        {Pid, #inotify_event{filename=FullPath, mask=[delete_self]}} ->
            ct:log("ok")
    after
        100 ->
            print_received(),
            throw(timeout)
    end,
    
    process_flag(trap_exit, Val),
    ok.

print_received() ->
    receive
        Event ->
            Val = io_lib:format("~p~n", [Event]),
            ct:log(Val)
    after 200 ->
            throw(too_long)
    end.
