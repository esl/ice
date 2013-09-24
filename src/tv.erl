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
        undefined ->
            {error, {unable_to_pass,server_down}};
        _ ->
            icy:pass({Name, Thing})
    end.

%% Internals

%% End of Module.
