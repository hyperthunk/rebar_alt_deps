%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(rebar_alt_deps).

-export(['install-deps'/2]).
-export(['post_install-deps'/2]).

'install-deps'(Config, _) ->
    case rebar_config:get_local(Config, 'alt_install_trigger', false) of
        true ->
            rebar_core:process_commands(['get-deps'], Config);
        false ->
            ok
    end,
    {ok, DepsDir, Deps} = install_alt_remotes(Config),
    case file:list_dir(DepsDir) of
        {ok, Dirs} ->
            [ process_dir(Dir, Deps, Config) || Dir <- Dirs ];
        {error, Err} ->
            rebar_log:log(warn, "Unable to process deps: ~p~n", [Err])
    end,
    ok.

'post_install-deps'(Config, _) ->
    Cwd = rebar_utils:get_cwd(),
    BaseDir = rebar_config:get_global(base_dir, undefined),
    case Cwd == BaseDir of
        false ->
            ok;
        true ->
            AltDeps = rebar_config:get_local(Config, alt_tmp_dir,
                                             ".altdeps"),
            WorkDir = filename:join(Cwd, AltDeps),
            rebar_log:log(debug, "Cleaning up temp file(s) in ~s~n", [WorkDir]),
            rebar_file_utils:rm_rf(WorkDir)
    end,
    ok.

%%
%% Internal API
%%

process_dir(Dir, Deps, Config) ->
    Origin = rebar_utils:get_cwd(),
    %rebar_log:log(debug, "Processing ~s~n", [Dir]),
    Basename = filename:basename(Dir),
    Command = case ([ BuildCmd || {App, _Vsn, BuildCmd} <- Deps,
                                atom_to_list(App) == Basename andalso 
                                is_atom(BuildCmd) ]) of
        [] -> compile;
        undefined -> compile;
        [Cmd|_] -> Cmd
    end,
    try
        rebar_log:log(info, "Executing ~p in ~s~n", [Command, Dir]),
        file:set_cwd(Dir),
        rebar_core:skip_dir(Origin),
        rebar_core:process_commands([Command], Config),
        erlang:erase({skip_dir, Origin})
    catch _:Err ->
        rebar_log:log(warn, "Unable to process directory ~s: ~p~n", [Dir, Err])
    after
        file:set_cwd(Origin)
    end.

install_alt_remotes(Config) ->
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    {Deps, Found} = find_deps(DepsDir, Config),
    Missing = [ Spec || {false, Spec, _} <- Found ],
    rebar_log:log(info, "Missing alt-deps: ~p~n",
                  [[ M || M <- Missing ]]),
    install_missing_deps(DepsDir, Missing, [], Config),
    {ok, DepsDir, Deps}.

find_deps(DepsDir, Config) ->
    RebarDeps = rebar_config:get_local(Config, deps, []),
    AltDeps = rebar_config:get_local(Config, alt_deps, []),

    %% we do not handle deps with a given source directory - these are
    %% dealt with by rebar as usual
    ManagedDeps = [ load_if_possible(DepsDir, AppVsn) ||
                                        {_App, _Vsn}=AppVsn <- RebarDeps ],

    {RebarDeps ++ AltDeps,
        [ load_if_possible(DepsDir, AppVsn) ||
                        AppVsn <- AltDeps ] ++ ManagedDeps}.

load_if_possible(DepsDir, {App, _, _}=Spec) ->
    BasePath = filename:join(DepsDir, atom_to_list(App)),
    rebar_log:log(debug, "Searching ~s for ~p~n", [BasePath, App]),
    case filelib:is_dir(BasePath) of
        true ->
            BasePath;
        false ->
            {false, Spec, missing}
    end;
load_if_possible(DepsDir, {App, Vsn}=Spec) ->
    BasePath = filename:join(DepsDir, atom_to_list(App)),
    rebar_log:log(debug, "Searching ~s for ~p-~s~n", [BasePath, App, Vsn]),
    case filelib:wildcard(BasePath ++ "*") of
        [SubDir|_] ->
            case is_app_available(App, Vsn, SubDir) of
                {true, _}=Found ->
                    Found;
                {false, Info} ->
                    {false, Spec, Info}
            end;
        _ ->
            {false, Spec, missing}
    end.

install_missing_deps(_DepsDir, [], Loaded, _Config) ->
    Loaded;
install_missing_deps(DepsDir, [Spec|Missing], SoFar, Config) ->
    AltConfig = rebar_config:get_local(Config, alt_repositories, []),
    App = element(1, Spec),
    {App, AltDep} = lists:keyfind(App, 1, AltConfig),
    Loaded = alt_load(DepsDir, Spec, AltDep, Config),
    install_missing_deps(DepsDir, Missing, [Loaded|SoFar], Config).

%% TODO: add support for nexus
%% TODO: add support for erlware repos
%% TODO: add support for explicit URLs
%% TODO: integrate rebar_deps support for git/hg/svn

alt_load(_, Spec, false, _) ->
    {noconfig, Spec};
alt_load(DepsDir, {_, Vsn, _}=AppSpec, {Repo, Author}, Config) ->
    alt_load(DepsDir, AppSpec, {Repo, Author, Vsn}, Config);
alt_load(DepsDir, {_, Vsn}=AppSpec, {Repo, Author}, Config) ->
    alt_load(DepsDir, AppSpec, {Repo, Author, Vsn}, Config);
alt_load(DepsDir, {App, Vsn}, {Repo, Author, Tag}, Config) ->
    alt_load(DepsDir, {App, Vsn, undefined}, {Repo, Author, Tag}, Config);
alt_load(DepsDir, {App, Vsn, _}, {Repo, Author, Tag}, Config) ->
    rebar_log:log(debug, "Loading ~p-~p from ~p~n",
                  [App, Vsn, Repo]),
    WorkDir = filename:join(rebar_utils:get_cwd(),
                    rebar_config:get_local(Config, alt_tmp_dir, ".altdeps")),
    AppName = atom_to_list(App) ++ "-" ++ Vsn,
    Target = filename:join(WorkDir, Author ++ "-" ++ AppName),
    Url = make_url(Repo, Author, App, Tag),
    case fetch(Url, Target, Config) of
        {error, Error} ->
            rebar_log:log(warn, "Unable to load ~s: ~p~n", [AppName, Error]);
        {ok, Target} ->
            case zip:extract(Target, [memory]) of
                {error, Reason} ->
                    rebar_log:log(warn,
                                  "Unable to extract archive ~s: ~p~n",
                                  [Target, Reason]);
                {ok, FileList} ->
                    Dest = filename:join(DepsDir, atom_to_list(App)),
                    [ file:write_file(rename(Path, Dest), Data, [write]) ||
                                                    {Path, Data} <- FileList ],
                    %[ rebar_file_utils:mv(F,
                    %    rename(F, Dest, WorkDir)) || F <- FileList ],
                    %case Skip of
                    %    undefined ->
                    %        ok;
                    %    _ ->
                    %        {exclude, filename:basename(Dest)}
                    %end
                    
                    ok
            end
    end.

rename(Path, Dest) ->
    [_|P] = filename:split(Path),
    Target = filename:join(Dest, filename:join(P)),
    rebar_utils:ensure_dir(Target),
    Target.

%rename(FN, Dest, WorkDir) ->
%    NewFN = filename:join([Dest] ++
%        erlang:tl(filename:split(FN -- (WorkDir ++ "/" )))),
%    rebar_utils:ensure_dir(NewFN),
%    NewFN.

make_url(bitbucket, Author, App, Vsn) ->
    "https://bitbucket.org/" ++ Author ++ "/" ++
    atom_to_list(App) ++ "/get/" ++ Vsn ++ ".zip";
make_url(github, Author, App, Vsn) ->
    "https://github.com/" ++ Author ++ "/" ++
    atom_to_list(App) ++ "/zipball/" ++ Vsn.

%matching_dirs(F, ManagedDeps) ->
%    lists:filter(fun({App, _Vsn}) ->
%                    lists:prefix(atom_to_list(App), F)
%                end, ManagedDeps).

is_app_available(App, VsnRegex, Path) ->
    rebar_log:log(debug, "Searching for ~p(~~ ~s) in ~s~n", [App, VsnRegex, Path]),
    case rebar_app_utils:is_app_dir(Path) of
        {true, AppFile} ->
            case rebar_app_utils:app_name(AppFile) of
                App ->
                    Vsn = rebar_app_utils:app_vsn(AppFile),
                    case re:run(Vsn, VsnRegex, [{capture, none}]) of
                        match ->
                            {true, Path};
                        nomatch ->
                            {false, {version_mismatch,
                                     {AppFile,
                                      {expected, VsnRegex}, {has, Vsn}}}}
                    end;
                OtherApp ->
                    {false, {name_mismatch,
                             {AppFile, {expected, App}, {has, OtherApp}}}}
            end;
        false ->
            {false, {missing_app_file, Path}}
    end.

fetch(Url, Target, Config) ->
    case get({?MODULE, httpc}) of
        started ->
            ok;
        _ ->
            inets:start(),
            application:load(sasl),
            application:set_env(sasl, sasl_error_logger, false),
            lists:map(fun application:start/1, [sasl, crypto, public_key, ssl]),
            Timeout = rebar_config:get_local(Config, remote_net_timeout, 6000),
            case rebar_config:get_local(Config, remote_proxy_host, undefined) of
                undefined ->
                    httpc:set_options([{timeout, Timeout},
                                       {connect_timeout, Timeout}]);
                Host ->
                    Port = rebar_config:get_local(Config,
                                                  remote_proxy_port, "8080"),
                    httpc:set_options([{proxy, {{Host, Port}, ["localhost"]}},
                                       {timeout, Timeout},
                                       {connect_timeout, Timeout}])
            end,
            put({?MODULE, httpc}, started)
    end,
    rebar_utils:ensure_dir(Target),
    Request = {Url, [{"User-Agent", "Erlang-Rebar"}]},
    rebar_log:log(debug, "Attempting to fetch ~s into ~s~n", [Url, Target]),
    case httpc:request(get, Request, [{relaxed, true}],
                                     [{stream, Target}, {full_result, true}]) of
        {ok, saved_to_file} ->
            rebar_log:log(debug, "Successfully fetched tag...~n", []),
            {ok, Target};
        {ok, {{_, 404, _}, _, _}} ->
            rebar_log:log(warn, "~s not found!~n", [Url]),
            {error, missing};
        Error ->
            rebar_log:log(warn, "Error trying to load remote: ~p~n", [Error]),
            {error, Error}
    end.

