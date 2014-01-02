-module(ice_dtree).

-include_lib("eunit/include/eunit.hrl").

-export([new/0, delete/0]).
-export([lookup/1,
         insert/2, insert_new/2]).
-export([sort_context/1]).

-define(TABLE_NAME, ice_cache).

-type id() :: nonempty_string().
-type dim() :: term().
-type ground_value() :: term().
-type context() :: [{dim(), ground_value()}].
-type calc() :: {calc, Pid :: term()}.
-type missing_dims() :: [dim(), ...].
-type k() :: {id(), context()}.

-record(?TABLE_NAME,
        {k :: k(),
         v :: calc() | {i, missing_dims()} | ground_value()}).

%%------------------------------------------------------------------------------
%% @doc Create a new ets table for the cache
%%------------------------------------------------------------------------------
new() ->
  ok = application:start(mnesia),
  {atomic, ok} =
    mnesia:create_table(
      ?TABLE_NAME,
      [{attributes, record_info(fields, ?TABLE_NAME)},
       {disc_copies, []},
       {disc_only_copies, []},
       {ram_copies, [node()]},
       {storage_properties, [{ets, [{read_concurrency, true},
                                    {write_concurrency, true}]}]},
       {type, set}]),
  ok.

%%------------------------------------------------------------------------------
%% @doc Delete the ets table
%%------------------------------------------------------------------------------
delete() ->
  {atomic, ok} = mnesia:delete_table(?TABLE_NAME),
  ok = application:stop(mnesia),
  ok.

%%------------------------------------------------------------------------------
%% @doc Lookup an {Identifier, Context} pair in the cache
%%
%% The specified context Key must be already sorted by dimension.
%%------------------------------------------------------------------------------
-spec lookup(k()) -> calc() | missing_dims() | ground_value().
lookup({Xi,Key} = XiKey) ->
  case mnesia:async_dirty(fun() -> mnesia:read(?TABLE_NAME, XiKey) end) of
    [] ->
      lookup({Xi,[]}, Key);
    [#?TABLE_NAME{v={i,Dims}}] ->
      Dims;
    [#?TABLE_NAME{v=Value}] ->
      Value
  end.

lookup({Xi,Key} = XiKey, K) ->
  case mnesia:async_dirty(fun() -> mnesia:read(?TABLE_NAME, XiKey) end) of
    [] ->
      [];
    [#?TABLE_NAME{v={calc,_W}=Value}] ->
      %%throw(error_calc);
      Value;
    [#?TABLE_NAME{v={i,Dims}}] ->
      case ice_sets:restrict_domain(K, Dims) of
        [] ->
          Dims;
        K1 ->
          lookup({Xi,sort_context(K1)}, ice_sets:subtract_by_domain(K, Dims))
      end;
    [#?TABLE_NAME{v=Value}] ->
      Value
  end.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, Key} into the cache
%%
%% The specified context Key must be already sorted by dimension.
%%------------------------------------------------------------------------------
-spec insert(k(), missing_dims() | ground_value()) -> true.
insert({_,_} = XiKey, Dims) when is_list(Dims) andalso length(Dims) > 0 ->
  insert(XiKey, {i,sort_dims(Dims)});
insert({_,_} = XiKey, V) ->
  {atomic, ok} =
    mnesia:transaction(
      fun() ->
          ok = mnesia:write(#?TABLE_NAME{k=XiKey, v=V})
      end),
  true.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, Key} into the cache unless already present
%%
%% The specified context Key must be already sorted by dimension.
%%------------------------------------------------------------------------------
-spec insert_new(k(), calc()) ->
                    {true, calc()} |
                    {false, calc() | missing_dims() | ground_value()}.
insert_new({_,_} = XiKey, {calc,_} = V) ->
  {atomic, {B, V2}} =
    mnesia:transaction(
      fun() ->
          case mnesia:read(?TABLE_NAME, XiKey) of
            [] ->
              ok = mnesia:write(#?TABLE_NAME{k=XiKey, v=V}),
              {true, V};
            [#?TABLE_NAME{v={i,Dims}}] ->
              {false, Dims};
            [#?TABLE_NAME{v=V1}] ->
              {false, V1}
          end
      end),
  {B, V2}.

%%------------------------------------------------------------------------------
%% @doc Sort the specified context by dimension
%%------------------------------------------------------------------------------
sort_context(Key) -> lists:keysort(1, Key).

%%------------------------------------------------------------------------------
%% @doc Sort the specified list of dimensions
%% @private
%%------------------------------------------------------------------------------
sort_dims(Dims) -> lists:sort(Dims).


insert_correct_tree() ->
  insert({"A",[]}, {i,[{dim,t}]}),
  insert({"A",[{{dim,t},0}]}, {i,[{dim,s}]}),
  insert({"A",[{{dim,t},0},{{dim,s},0}]}, 1),
  insert({"A",[{{dim,t},1}]}, {i,[{dim,s}]}),
  insert({"A",[{{dim,t},1},{{dim,s},0}]}, 2),
  insert({"A",[{{dim,t},1},{{dim,s},1}]}, 3),
  insert({"A",[{{dim,t},2}]}, {calc,[0]}),
  insert({"B",[]}, {i,[{dim,s}]}),
  insert({"B",[{{dim,s},0}]}, 1),
  insert({"B",[{{dim,s},1}]}, 1).

lookup_test() ->
  ok = ?MODULE:new(),
  insert_correct_tree(),
  [{dim,t}] = ?MODULE:lookup({"A",[]}),
  [{dim,s}] = ?MODULE:lookup({"A",[{{dim,t},0}]}),
  1 = ?MODULE:lookup({"A",[{{dim,t},0},{{dim,s},0}]}),
  [] = ?MODULE:lookup({"B",[{{dim,s},2}]}),
  [{dim,s}] = ?MODULE:lookup({"B",[{{dim,t},0}]}),
  1 = ?MODULE:lookup({"B",[{{dim,t},0},{{dim,s},0}]}),
  {calc,[0]} = ?MODULE:lookup({"A",[{{dim,t},2}]}),
  2 = ?MODULE:lookup({"A",[{{dim,t},1},{{dim,s},0}]}),
  3 = ?MODULE:lookup({"A",[{{dim,t},1},{{dim,s},1}]}),
  ok = ?MODULE:delete().
