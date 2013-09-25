%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tv).

%% tv: Tea Visualiser.

-export([hook/3]).

%% API

-type name() :: string() | atom().
%% Just call it like so: `tv:hook(?MODULE, "caching var", {D,A,Ta})`.
-spec hook (Module::name(), Descr::name(), Data::term()) -> any().
hook (Name, Descr, Data) ->
    case whereis(icy) of
        _P when is_pid(_P) ->
            icy:pass(Name, icy:time(), Descr, Data);
        _ ->
            {error, {unable_to_pass,server_down}}
    end.

%% Internals

%% End of Module.
