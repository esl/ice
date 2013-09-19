%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tv).

%% tv: Tea Visualiser.

-export([pass/1]).

%% API

pass (Thing) ->
    case whereis(icy) of
        undefined ->
            {error, {unable_to_pass,server_down}};
        _ ->
            icy ! {pass, Thing}
    end.

%% Internals

%% End of Module.
