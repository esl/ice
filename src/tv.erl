%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tv).

%% tv: Tea Visualiser.

-export([hook/2]).

%% API

%% Just call it like so: `tv:hook("caching var", {D,A,T,A2})`.
%%   Name can also  be ?MODULE or the name of the function the
%%   hook is in. It does not need to be unique.
-spec hook (string() | atom(), term()) -> any().
hook (Name, Thing) ->
    case whereis(icy) of
        _P when is_pid(_P) ->
            icy:pass(Name, icy:time(), Thing);
        _ ->
            {error, {unable_to_pass,server_down}}
    end.

%% Internals

%% End of Module.
