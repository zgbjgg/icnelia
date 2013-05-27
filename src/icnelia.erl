%
% ICNELIA : Helper for compile, clean and create run scripts for 
%	    an erlang OTP application
%
% Copyright (C) 2012-2013, Jorge Garrido <zgbjgg@gmail.com> All rights reserved.
%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%

-module(icnelia).

-export([main/1]).

-include("icnelia.hrl").

% main function, receives the option for a command
main(Opts) ->
    case catch(processing_cmd(icnelia_utils:all_atoms(Opts))) of
        {ok, _}            ->  
	    ok;
	{error, FileError} ->
	    syntax(FileError);
	_                  ->
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
"Usage ~s command ~n"
"  compile\t\t\tCompile all source code in 'src dir'~n"
"  clean\t\t\t\tCleans all ebin dir from compiled code (beams)~n"
"  runner\t\t\tCreates run script for this application~n"
"  runner daemon\t\t\tCreates run script for this application as daemon~n",
[filename:basename(escript:script_name())]),
    halt(1).

% error syntaxis 
syntax({Line, _Mod, Term}) ->
   io:fwrite("Syntax Error icnelia.config: ~n Line ~p~n ~p~n", [Line, Term]);
syntax(Other)              ->
   io:fwrite("Error Parsing icnelia.config: ~p~n", [Other]).
