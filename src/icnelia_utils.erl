%
% ICNELIA : Helper for compile, clean and create run scripts for 
%           an erlang OTP application
%
% Copyright (C) 2012, Jorge Garrido <jorge.garrido@morelosoft.com>
%
% All rights reserved.
%
-module(icnelia_utils).

-export([all_atoms/1, chmod/2, get_value/2, get_pa_string/1]).

% all elements in a list as atoms
all_atoms(List) ->
    [ list_to_atom(Elem) || Elem <- List ].

% set permissions for a file/script
chmod(File, Permissions) ->
    [] = os:cmd("chmod " ++ Permissions ++ " " ++ File).

% get value with key given 
get_value(KeyLookUp, List) ->
    case [ Value || {Key, Value} <- List, Key == KeyLookUp ] of
	[]        ->
	    [];
	[ Value ] ->
	    Value
    end.

% concat the string for pa runner script option
get_pa_string([]) ->
    "";
get_pa_string([E | Es]) ->
    "$" ++ E ++ " " ++ get_pa_string(Es).
