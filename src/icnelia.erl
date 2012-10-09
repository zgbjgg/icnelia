%
% ICNELIA : Helper for compile, clean and create run scripts for 
%	    an erlang OTP application
%
% Copyright (C) 2012, Jorge Garrido <jorge.garrido@morelosoft.com>
%
% All rights reserved.
%

-module(icnelia).

-export([main/1]).

-include("../include/icnelia.hrl").

% main function, receives the option for a command
main(Opts) ->
    process([list_to_atom(Opt) || Opt <- Opts]).

% process the option, pattern matching with the given option
process([runner, daemon | _])  ->
    _X = runner_daemon(get_value(app_deps, get_cfg())),
    os:cmd("chmod u+x " ++ ?run);
process([runner | _])          ->
    _X = runner(get_value(app_deps, get_cfg())),
    os:cmd("chmod u+x " ++  ?run);
process([clean | _])           ->
    {ok, _ } = clean();
process([compile | _])         ->
    {ok, _ } = compile(get_value(erl_opts, get_cfg()));
process(_)                     ->
    help().

% option compile
compile(undefined) ->
    compile([]);
compile(_opts)     ->
    Modules = get_files(?src_erl),
    AppSrc = get_files(?src_app_src),
    case AppSrc of
        []     ->
            ok;
        [File] ->
            AppName = get_app_name(),
            ok = file:rename(File, "ebin/" ++ AppName ++ ".app")
    end,
    Beams = [ begin
                case c:c(Module, [{i, ?include_dir}, {outdir, ?ebin_dir}] ++ _opts) of
                    error ->
                        halt(1);
                    R     ->
                        io:format(?msg_c, [Module]),
                        R
                end
              end || Module <- Modules ],
    {ok, Beams}.

% option clean
clean() ->
    Beams = get_files(?ebin_beam),
    case get_files(?src_app_ebin) of
        []    ->
            ok;
        [App] ->
            ok = file:delete(App)
    end,
    Delete = [ begin
                ok = file:delete(Beam),
                io:format(?msg_d, [Beam])
               end || Beam <- Beams ],
    {ok, Delete}.

% option runner
runner(undefined) ->
    runner([]);
runner(_opts)     ->
    {ok, IoDev} = file:open(?run, [write, append]),
% append lines for path to deps (ebin)
    [ io:format(IoDev, "~s=deps/~s~n", [Name, Value]) || {Name, Value} <- _opts ],
% get name for application
    App = get_app_name(),
% append line for the runner script
    io:format(IoDev, "erl -pa ebin/ " ++ get_pa_string([ Name || {Name, _} <- _opts ]) ++
                        " -eval 'application:start(" ++ App ++ ").'", []).

% option runner daemon
runner_daemon(undefined) ->
    runner_daemon([]);
runner_daemon(_opts)     ->
    {ok, IoDev} = file:open(?run, [write, append]),
% append lines for path to deps (ebin)
    [ io:format(IoDev, "~s=deps/~s~n", [Name, Value]) || {Name, Value} <- _opts ],
% get name for application
    App = get_app_name(),
% get pipes and logs directories
    Pipes = get_value(pipes_dir, get_cfg()),
    Logs = get_value(logs_dir, get_cfg()),
% append line for the runner script
    io:format(IoDev, "run_erl -daemon " ++ Pipes ++ " " ++ Logs ++ " exec \"erl -pa ebin/ " ++ get_pa_string([ Name || {Name, _} <- _opts ]) ++
                        " -eval 'application:start(" ++ App ++ ").'\"", []).

% get files by wildcard
get_files(Wildcard) ->
    lists:filter(fun(X) -> filelib:is_file(X) end, filelib:wildcard(Wildcard)).

% get config on meen.config
get_cfg() ->
    case file:consult(?cfg_file) of
        {error, enoent}    ->
            [];
        {error, FileError} ->
            io:format("~s: ~s ~n", [?cfg_file, FileError]),
            halt(2);
        {ok, List}         ->
            List
    end.

% get value for key given
get_value(KeyLookUp, List) ->
    Val = [ Value || {Key, Value} <- List, Key == KeyLookUp ],
    case Val of [] -> undefined; [Value] -> Value end.

% get string from a list with many elements
get_pa_string([])      ->
    "";
get_pa_string([H | T]) ->
    "$" ++ H ++ " " ++ get_pa_string(T).

% get application name
get_app_name() ->
    [File | _] = get_files(?src_app_src),
    {ok, [{application, App, _}]} = file:consult(File),
    atom_to_list(App).

% help or icnelia?
help() ->
    io:fwrite(
"Usage ~s Cmd[Opts] ~n"
"Valid commands and options~n"
"  compile     		: compiles all in src dir and place under ebin dir~n"
"  clean                : cleans all in ebin dir (compiled)~n"
"  runner       	: creates a script to run the application~n"
"  runner daemon        : creates a script to run the application as daemon~n",
[filename:basename(escript:script_name())]),
    halt(1).
