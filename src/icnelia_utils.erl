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

-module(icnelia_utils).

-export([all_atoms/1, chmod/2, get_value/2, get_pa_string/1]).

%% @doc all elements in a list as atoms
%% @spec all_atoms([string()]) -> [atom()]
-spec all_atoms([string()]) -> [atom()].
all_atoms(List) ->
    [ list_to_atom(Elem) || Elem <- List ].

%% @doc set permissions for a file/script under unix, win32
%% @spec chmod(string(),string()) -> []
-spec chmod(string(),string()) -> [].
chmod(File, Permissions) ->
    chmod(File, Permissions, os:type()).

%% @doc set permissions for a file/script under unix, win32
%% @spec chmod(File :: string(), Permissions :: string(), {unix, string()} | {win32, string()}) -> list()
-spec chmod(File :: string(), Permissions :: string(), {unix, string()} | {win32, string()}) -> list().
chmod(File, Permissions, {unix, _})    ->
    [] = os:cmd("chmod " ++ Permissions ++ " " ++ File);
chmod(_File, _Permissions, {win32, _}) ->
    [].

%% @doc get value with key given 
%% @spec get_value(KeyLookUp :: term(), List :: list()) -> Value :: term()
-spec get_value(KeyLookUp :: term(), List :: list()) -> Value :: term().
get_value(KeyLookUp, List) ->
    case [ Value || {Key, Value} <- List, Key == KeyLookUp ] of
	[]        ->
	    [];
	[ Value ] ->
	    Value
    end.

%% @doc concat the string for pa runner script option
%% @spec get_pa_string(Es :: list()) -> string()
-spec get_pa_string(Es :: list()) -> string().
get_pa_string([]) ->
    "";
get_pa_string([E | Es]) ->
    "$" ++ E ++ " " ++ get_pa_string(Es).
