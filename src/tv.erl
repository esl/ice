%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tv).

%% tv: Tea Visualiser.

-export([hook/4]).

%% API

-type name() :: string() | atom().
%% Just call it like so: `tv:hook(?MODULE, self(), "caching var", {D,A,Ta})`.
-spec hook (Module::name(), Pid::pid(), Descr::name(), Data::term()) -> any().
hook (Module, Pid, Descr, Data) ->
    Tv = isee,
    case whereis(Tv) of
        P when is_pid(P) ->
            Tv:pass(Module, Pid, Tv:time(), Descr, Data);
        _ ->
            {error, {unable_to_pass,server_down}}
    end.

%% Internals

%% End of Module.
