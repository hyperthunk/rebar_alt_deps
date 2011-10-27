-module(t_simple_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy,
        "../../examples/simple", "simple"},
     {copy, "rebar.config", "simple/rebar.config"}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    ?assertMatch({ok, _}, retest:sh("rebar install-deps -v", 
                [{dir, "simple"}])),
    ?assertMatch(true, filelib:is_dir("simple/deps/rebar_alien_plugin")),
    ?assertMatch(true, filelib:is_dir("simple/deps/remote_plugin_loader")),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

