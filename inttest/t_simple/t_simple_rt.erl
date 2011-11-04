-module(t_simple_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy,
        "../../examples/simple", "simple"},
     {copy, "rebar.config", "simple/rebar.config"},
     {copy, "mydep.config", "mydep/rebar.config"},
     {create, "mydep/src/mydep.app", app(mydep)}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    ?assertMatch({ok, _}, retest:sh("git init", [{dir, "mydep"}])),
    ?assertMatch({ok, _}, retest:sh("git add rebar.config", [{dir, "mydep"}])),
    ?assertMatch({ok, _}, retest:sh("git ci -m \"initial commit\"'", [{dir, "mydep"}])),
    ?assertMatch({ok, _}, retest:sh("rebar install-deps -v",
                [{dir, "simple"}])),
    ?assertMatch(true, filelib:is_dir("simple/deps/rebar_alien_plugin-0.0.2")),
    ?assertMatch(true, filelib:is_dir("simple/deps/remote_plugin_loader-0.0.2")),
    ?assertMatch(true, filelib:is_dir("simple/deps/mydep-1")),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

