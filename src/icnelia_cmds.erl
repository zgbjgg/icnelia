%
% ICNELIA : Helper for compile, clean and create run scripts for 
%           an erlang OTP application
%
% Copyright (C) 2012-2013, Jorge Garrido <zgbjgg@gmail.com> All rights reserved.
%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%

-module(icnelia_cmds).

-export([cmd/1]).

-include("../include/icnelia.hrl").

%% @doc command selector
%% @speccmd(atom()) -> {'error',_} | {'ok',[any()]}
-spec cmd(atom()) -> {'error',_} | {'ok',[any()]}.
cmd(Cmd) ->
    Config = icnelia_files:get_file_config(),
    cmd(Cmd, {config, Config}).

%% @doc internal command selector
%% @spec cmd(Cmd :: atom(), {config, {error, term()} | Config :: list()}) -> {ok, term()} | {error, term()}
-spec cmd(Cmd :: atom(), {config, Config :: list()}) -> {ok, term()} | {error, term()}.
cmd(_Cmd, {config, {error, FileError}}) ->
    {error, FileError};
cmd(Cmd, {config, Config})             ->
    case Cmd of
	compile  -> compile(Config);
	clean    -> clean(Config);
	runner   -> runner(Config);
	runner_d -> runner_d(Config)
    end.

%% @doc compile command
%% @spec compile(Config :: list()) -> {ok, Beams :: list()}
-spec compile(Config :: list()) -> {ok, Beams :: list()}.
compile(Config) ->
    ErlOpts = icnelia_utils:get_value(erl_opts, Config),
    Modules = icnelia_files:get_files(?src_erl),
% ensure that ebin directory exists, otherwise create it!!
    ok = case filelib:is_dir(?ebin_dir) of
	     true  -> 
	         ok;
	     false ->
		 file:make_dir(?ebin_dir)
	 end,
    case icnelia_files:get_files(?src_app_src) of
	[]       ->
	    ok;
	[ File ] ->
	    AppName = icnelia_files:get_app_name(),
            {ok, Binary} = file:read_file(File),
	    ok = file:write_file(?ebin_dir ++ AppName ++ ".app", Binary)
    end,
    Beams = [ begin
		case c:c(Module, [{i, ?include_dir}, {outdir, ?ebin_dir}] ++ ErlOpts) of
		    error ->
			halt(1);
		    C     ->
			io:format(?msg_c, [Module]),
			C
		end
	      end || Module <- Modules ],
    {ok, Beams}.

%% @doc clean command
%% @spec clean([any()]) -> {'ok',['ok']}
-spec clean([any()]) -> {'ok', ['ok']}.
clean(_Config) ->
    Beams = icnelia_files:get_files(?ebin_beam),
    case icnelia_files:get_files(?src_app_ebin) of
	[]          ->
   	    ok;
	[ AppFile ] ->
	    ok = file:delete(AppFile)
    end,
    Removed = [ begin
		    ok = file:delete(Beam),
		    io:format(?msg_d, [Beam])
		end || Beam <- Beams ],
    {ok, Removed}.
 
%% @doc runner command
%% @spec runner([any()]) -> {'ok',[46 | 97 | 98 | 110 | 114 | 116 | 117,...]}
-spec runner([any()]) -> {'ok',[46 | 97 | 98 | 110 | 114 | 116 | 117,...]}.
runner(Config) ->
    AppDeps = icnelia_utils:get_value(app_deps, Config),
    {ok, IoDev} = icnelia_files:get_run_script(),
%% append lines used as commentaries in script
    a_comments(deps, IoDev),
% append lines for path to deps (ebin)
    _ = [ io:format(IoDev, "~s=~s~n", [Name, Path]) || {Name, Path} <- AppDeps ],
    AppName = icnelia_files:get_app_name(),
%% append lines used as commentaries in script
    a_comments(startup, IoDev),    
    io:format(IoDev, ?runner(icnelia_utils:get_pa_string([ Name || {Name, _} <- AppDeps ]), AppName), []),
    [] = icnelia_utils:chmod(?run, ?u_x),
    {ok, ?run}.

%% @doc runner daemon command
%% @spec runner_d([any()]) -> {'ok',[46 | 97 | 98 | 110 | 114 | 116 | 117]}
-spec runner_d([any()]) -> {'ok',[46 | 97 | 98 | 110 | 114 | 116 | 117]}.
runner_d(Config) ->
    runner_d(Config, os:type()).

%% @doc runner daemon command
%% @spec runner_d(Config :: list(), {unix, string()} | {win32, string()}) -> {ok, ""}
-spec runner_d(Config :: list(), {unix, string()} | {win32, string()}) -> {ok, string()}.
runner_d(Config, {unix, _}) ->
    AppDeps = icnelia_utils:get_value(app_deps, Config),
    {ok, IoDev} = icnelia_files:get_run_script(),
%% append lines used as commentaries in script
    a_comments(deps, IoDev),
    _ = [ io:format(IoDev, "~s=~s~n", [Name, Path]) || {Name, Path} <- AppDeps ],
    AppName = icnelia_files:get_app_name(),
    [Pipes, Logs] = [ icnelia_utils:get_value(Key, Config) || Key <- [pipes_dir, logs_dir] ],
%% append lines used as commentaries in script
    a_comments(startup, IoDev),    
    io:format(IoDev, ?runner_d(icnelia_utils:get_pa_string([ Name || {Name, _} <- AppDeps ]), AppName, Pipes, Logs), []),     
    [] = icnelia_utils:chmod(?run, ?u_x),
    {ok, ?run};
runner_d(_Config, {win32, _}) ->
    io:format("Cannot create a daemon script on WIN32 SYSTEM\n"),
    {ok, ""}.

%% @doc append comments
%% @spec a_comments('deps' | 'startup',pid()) -> 'ok'
-spec a_comments('deps' | 'startup',pid()) -> 'ok'.
a_comments(Type, IoDev) ->
    a_comments(Type, IoDev, os:type()).

%% @doc append comments
%% @spec a_comments(Type :: atom(), IoDev :: pid(), {unix, string()} | {win32, string()}) -> ok
-spec a_comments(Type :: atom(), IoDev :: pid(), {unix, string()} | {win32, string()}) -> ok.
a_comments(deps, IoDev, {unix, _})     ->
    io:format(IoDev, "# Deployment Application Settings\n#-------------------------------------------------\n", []);
a_comments(deps, IoDev, {win32, _})    ->
    io:format(IoDev, "@echo off~nREM Deployment Application Settings~nREM -------------------------------------------------~n", []);
a_comments(startup, IoDev, {unix, _})  ->
    io:format(IoDev, "\n\n# Startup\n#-------------------------------------------------\n", []);
a_comments(startup, IoDev, {win32, _}) ->
    io:format(IoDev, "~n~nREM Startup~nREM -------------------------------------------------~n", []).
