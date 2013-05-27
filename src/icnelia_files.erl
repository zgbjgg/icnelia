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


-module(icnelia_files).

-export([get_file_config/0, get_files/1, get_app_name/0,
	 get_run_script/0]).

-include("../include/icnelia.hrl").

%% @doc get config from icnelia.config file
%% @spec get_file_config() -> list() | {error, term()}
-spec get_file_config() -> list() | {error, term()}.
get_file_config() ->
    case file:consult(?config_file) of
	% use a default configuration
	{error, enoent}    ->
	    [];
	% error to process file
	{error, FileError} ->
	    {error, FileError};
	% use the configuration given in icnelia.config
	{ok, Config}       ->
	    Config
    end.

%% @doc get files by wildcard
%% @spec get_files(Wildcard :: string()) -> list()
-spec get_files(Wildcard :: string()) -> list().
get_files(Wildcard) ->
    lists:filter(fun(X) -> filelib:is_file(X) end, filelib:wildcard(Wildcard)).

%% @doc get application name 
%% @spec get_app_name() -> string()
-spec get_app_name() -> string().
get_app_name() ->
    [ File ] = ?MODULE:get_files(?src_app_src),
    {ok, [{application, App, _}]} = file:consult(File),
    atom_to_list(App).

%% @doc get run script process to write on it
%% @spec get_run_script() -> {ok, pid()}
-spec get_run_script() -> {ok, pid()}.
get_run_script() ->
    file:open(?run, [write]).
