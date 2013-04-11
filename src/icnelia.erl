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

-include("icnelia.hrl").

% main function, receives the option for a command
main(Opts) ->
    try
        {ok, _} = processing_cmd(icnelia_utils:all_atoms(Opts))
    catch 
	_E:_R ->
	    help()
   end.

% process the cmd, pattern matching with the given option
processing_cmd([ runner, daemon ])  ->
    icnelia_cmds:cmd(runner_d);
processing_cmd([ runner ])          ->
    icnelia_cmds:cmd(runner);
processing_cmd([ clean ])           ->
    icnelia_cmds:cmd(clean);
processing_cmd([ compile ])         ->
    icnelia_cmds:cmd(compile).

% help or icnelia?
help() ->
    io:fwrite(
"Usage ~s cmd ~n"
"Valid commands~n"
"  compile     		: compiles all in src dir and place under ebin dir~n"
"  clean                : cleans all in ebin dir (compiled)~n"
"  runner       	: creates a script to run the application~n"
"  runner daemon        : creates a script to run the application as daemon~n",
[filename:basename(escript:script_name())]),
    halt(1).
