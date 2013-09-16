-module(tcache).

-behaviour(gen_server).

%% tcache API exports
-export([find/5, add/6, collect/0, stop/0]).

%% gen_server API exports
-export([start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, init/1, terminate/2]).

%%------------------------------------------------------------------------------
%% State
%%------------------------------------------------------------------------------
-record(state, { 
	  ck    = 0,
	  age   = 2,
	  data  = ets:new(tcache, []),
	  limit = undefined
	 }).

%%------------------------------------------------------------------------------
%% gen_server API implementation
%%------------------------------------------------------------------------------
start_link(Limit) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Limit], []).

init([Limit]) ->
  {ok, #state{ limit = Limit }}.

terminate(_, _) ->
  ok.

%%------------------------------------------------------------------------------
%% tcache API
%%------------------------------------------------------------------------------
find(X, K, D, W, T) ->
  gen_server:call(?MODULE, {find, X, K, D, W, T}).

add(X, K, D, W, T, V) ->
  gen_server:call(?MODULE, {add, X, K, D, W, T, V}).

collect() ->
  gen_server:call(?MODULE, collect).

stop() ->
  gen_server:call(?MODULE, terminate).

%%------------------------------------------------------------------------------
%% API implementation
%%------------------------------------------------------------------------------
handle_call({find, X, K, D, W0, T}, _From, S0) when T > S0#state.ck ->
  S1 = S0#state{ ck = T }, %% advance B.ck to t
  find_update(X, K, D, W0, T, S1);
handle_call({find, X, K, D, W0, T}, _From, S0) ->
  find_update(X, K, D, W0, T, S0);

handle_call({add, X, K, D, W, T, V}, _From, S0) when T > S0#state.ck ->
  S1 = S0#state{ ck = T }, 
  add_update(X, K, D, W, T, V, S1);
handle_call({add, X, K, D, W, T, V}, _From, S0) ->
  add_update(X, K, D, W, T, V, S0);

handle_call(collect, _From, S0) ->
  {reply, ok, S0};

handle_call(terminate, _From, S0) ->
  {stop, normal, ok, S0}.
%%------------------------------------------------------------------------------
%% internal
%%------------------------------------------------------------------------------
find_update(X, K, D, W0, _T, S0) ->
  Ko = lists:keysort(1, K),
  Do = lists:keysort(1, D),
  case ets:lookup(S0#state.data, {X,Ko,Do}) of
    [] ->
      Ks = [{Key,Ord} || {{_,Key},Ord} <- Ko],
      Ds = [Key || {_,Key} <- Do],
      io:format(user, "Adding ~p ~p ~p = ~p~n", [X,Ks,Ds,{calc,W0}]),
      true = ets:insert_new(S0#state.data, {{X,Ko,Do}, {calc, W0}}),
      S2 = S0#state{ck = S0#state.ck + 1},
      {reply, {{calc, W0}, S2#state.ck},  S2};
    [{_, {calc, W1} = V}] ->
      case W1 =< W0 of
	true ->
	  io:format(user, "Thread ~p is less than or equal to ~p~n", [W1, W0]),
	  {reply, hang, S0};
	false ->
	  {reply, {V, S0#state.ck}, S0}
      end;
    [{_, V}] ->
      {reply, {V, S0#state.ck}, S0};
    [_,_|_] ->
      io:format(user, "Multiple objects with the same key~n", []),
      {reply, hang, S0}
  end.

add_update(X, K, D, W, _T, V1, S0) ->
  Ko = lists:keysort(1, K),
  Do = lists:keysort(1, D),
  case ets:lookup(S0#state.data, {X,Ko,Do}) of
    [] ->
      {reply, hang, S0};
    [{_, {calc, W}}] ->
      Ks = [{Key,Ord} || {{_,Key},Ord} <- Ko],
      Ds = [Key || {_,Key} <- Do],
      io:format(user, "Adding ~p ~p ~p = ~p~n", [X,Ks,Ds,V1]),
      ets:insert(S0#state.data, {{X,Ko,Do}, V1}),
      S1 = S0#state{ck = S0#state.ck + 1},
      {reply, {V1, S1#state.ck}, S1};
    [{_, {calc, _W1}}] ->
      {reply, hang, S0};
    [O1,_O2|_Os] ->
      io:format(user, "Multiple objects with the same key ~p ~p~n", [W, O1]),
      {reply, hang, S0};
    Other ->
      [{{_X1,_K1,_D1},V1}] = Other,
      io:format(user, "Other = ~p, W = ~p~n", [Other, W]),
      {reply, hang, S0}
  end.

%%------------------------------------------------------------------------------
%% Not implemented
%%------------------------------------------------------------------------------
code_change(_, _, _) ->
  not_implemented.

handle_cast(_, _) ->
  not_implemented.

handle_info(_, _) ->
  not_implemented.
