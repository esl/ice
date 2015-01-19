-module(ice_dtree).

-export([new/0, delete/0]).
-export([lookup/1,
         insert/2, insert_new/2]).
-export([sort_context/1]).

-define(TABLE_NAME, ice_cache).

%%------------------------------------------------------------------------------
%% @doc Create a new ets table for the cache
%%------------------------------------------------------------------------------
new() ->
  Tab = ets:new(?TABLE_NAME, [named_table, public, set, 
			      {read_concurrency, true},
			      {write_concurrency, false},
			      {keypos, 1}]),
  ets:delete_all_objects(Tab),
  Tab.

%%------------------------------------------------------------------------------
%% @doc Delete the ets table
%%------------------------------------------------------------------------------
delete() ->
  ets:delete(?TABLE_NAME).

%%------------------------------------------------------------------------------
%% @doc Lookup an {Identifier, Context} pair in the cache
%%------------------------------------------------------------------------------
lookup({Xi,Key} = XiKey) ->
  case ets:lookup(?TABLE_NAME, XiKey) of
    [] ->
      lookup({Xi,[]}, Key);
    [{{_,_},{i,Dims,_}}] ->
      Dims;
    [{{_,_},Value}] ->
      Value
  end.

lookup({Xi,Key} = XiKey, K) ->
  case ets:lookup(?TABLE_NAME, XiKey) of
    [] ->
      [];
    [{{_,_},{calc,W}}] ->
      %%throw(error_calc);
      {calc, W};
    [{{_,Key},{i,Dims,_}}] ->
      case ice_sets:restrict_domain(K, Dims) of
	[] ->
	  Dims;
	K1 ->
	  lookup({Xi,sort_context(K1)}, ice_sets:subtract_by_domain(K, Dims))
      end;
    [{{_,_},Value}] ->
      Value
  end.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, Key} into the cache
%%------------------------------------------------------------------------------
insert({_,_} = XiKey, Dims) when is_list(Dims) andalso length(Dims) > 0 ->
  insert(XiKey, {i,sort_dims(Dims)});
insert({Xi,Key},V) ->
  ets:insert(?TABLE_NAME, {{Xi,Key},V}).

insert_new({_,_} = XiKey, {calc,_} = V) ->
      case ets:lookup(?TABLE_NAME, XiKey) of
            [] ->
               ets:insert(?TABLE_NAME, {XiKey,V}),
              {true, V};
             [{{_,_},{i,Dims}}] ->
              {false, Dims};
          [{{_,_},Value}] ->
              {false, Value}
          end.

sort_context(Key) -> lists:keysort(1, Key).

sort_dims(Dims) -> lists:sort(Dims).
