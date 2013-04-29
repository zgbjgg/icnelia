%
% ICNELIA : Helper for compile, clean and create run scripts for 
%           an erlang OTP application
%
% Copyright (C) 2012, Jorge Garrido <jorge.garrido@morelosoft.com>
%
% All rights reserved.
%
-module(icnelia_cmds).

-export([cmd/1]).

-include("../include/icnelia.hrl").

% command selector 
cmd(Cmd) ->
    Config = icnelia_files:get_file_config(),
    case Cmd of
	compile  -> compile(Config);
	clean    -> clean(Config);
	runner   -> runner(Config);
	runner_d -> runner_d(Config)
    end.

% compile cmd
compile(Config) ->
    ErlOpts = icnelia_utils:get_value(erl_opts, Config),
    Modules = icnelia_files:get_files(?src_erl),
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

% clean cmd
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
 
% runner cmd
runner(Config) ->
    AppDeps = icnelia_utils:get_value(app_deps, Config),
    {ok, IoDev} = icnelia_files:get_run_script(),
%% append lines used as commentaries in script
    a_comments(deps, IoDev),
% append lines for path to deps (ebin)
    [ io:format(IoDev, "~s=~s~n", [Name, Path]) || {Name, Path} <- AppDeps ],
    AppName = icnelia_files:get_app_name(),
%% append lines used as commentaries in script
    a_comments(startup, IoDev),    
    io:format(IoDev, ?runner(icnelia_utils:get_pa_string([ Name || {Name, _} <- AppDeps ]), AppName), []),
    [] = icnelia_utils:chmod(?run, ?u_x),
    {ok, ?run}.

% runner daemon cmd
runner_d(Config) ->
    runner_d(Config, os:type()).

runner_d(Config, {unix, _}) ->
    AppDeps = icnelia_utils:get_value(app_deps, Config),
    {ok, IoDev} = icnelia_files:get_run_script(),
%% append lines used as commentaries in script
    a_comments(deps, IoDev),
    [ io:format(IoDev, "~s=~s~n", [Name, Path]) || {Name, Path} <- AppDeps ],
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

% append comments
a_comments(Type, IoDev) ->
    a_comments(Type, IoDev, os:type()).

a_comments(deps, IoDev, {unix, _})     ->
    io:format(IoDev, "# Deployment Application Settings\n#-------------------------------------------------\n", []);
a_comments(deps, IoDev, {win32, _})    ->
    io:format(IoDev, "@echo off~nREM Deployment Application Settings~nREM -------------------------------------------------~n", []);
a_comments(startup, IoDev, {unix, _})  ->
    io:format(IoDev, "\n\n# Startup\n#-------------------------------------------------\n", []);
a_comments(startup, IoDev, {win32, _}) ->
    io:format(IoDev, "~n~nREM Startup~nREM -------------------------------------------------~n", []).
