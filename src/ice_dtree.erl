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
  case mnesia_non_transaction(fun() -> mnesia:read(?TABLE_NAME, XiKey) end) of
    [] ->
      lookup({Xi,[]}, Key);
    [#?TABLE_NAME{v={i,Dims}}] ->
      Dims;
    [#?TABLE_NAME{v=Value}] ->
      Value
  end.

lookup({Xi,Key} = XiKey, K) ->
  case mnesia_non_transaction(fun() -> mnesia:read(?TABLE_NAME, XiKey) end) of
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
insert({Xi,Key} = XiKey, V) ->
  Fun = fun() -> ok = mnesia:write(#?TABLE_NAME{k=XiKey, v=V}) end,
  %% ok = mnesia_transaction(Fun),
  ok = mnesia_transaction2(Fun, {"insert (i.e. add) Xi = ~p, Key = ~1000p", [Xi,Key]}),
  true.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, Key} into the cache unless already present
%%
%% The specified context Key must be already sorted by dimension.
%%------------------------------------------------------------------------------
-spec insert_new(k(), calc()) ->
                    {true, calc()} |
                    {false, calc() | missing_dims() | ground_value()}.
insert_new({Xi,Key} = XiKey, {calc,_} = V) ->
  Fun = fun() ->
            case mnesia:read(?TABLE_NAME, XiKey) of
              [] ->
                ok = mnesia:write(#?TABLE_NAME{k=XiKey, v=V}),
                {true, V};
              [#?TABLE_NAME{v={i,Dims}}] ->
                {false, Dims};
              [#?TABLE_NAME{v=V1}] ->
                {false, V1}
            end
        end,
  %% {_B, _V2} = mnesia_transaction(Fun).
  {_B, _V2} = mnesia_transaction2(Fun, {"insert_new (i.e. find) Xi = ~p, Key = ~1000p", [Xi,Key]}).

%%------------------------------------------------------------------------------
%% @doc Sort the specified context by dimension
%%------------------------------------------------------------------------------
sort_context(Key) -> lists:keysort(1, Key).

%%------------------------------------------------------------------------------
%% @doc Sort the specified list of dimensions
%% @private
%%------------------------------------------------------------------------------
sort_dims(Dims) -> lists:sort(Dims).

%%------------------------------------------------------------------------------
%% Mnesia helpers
%%------------------------------------------------------------------------------
mnesia_non_transaction(Fun) ->
  mnesia:async_dirty(Fun).

mnesia_transaction(Fun) ->
  {atomic, ResultOfFun} = mnesia:transaction(Fun),
  ResultOfFun.

%%------------------------------------------------------------------------------
%% Mnesia transaction restarts tracing
%%------------------------------------------------------------------------------
mnesia_transaction2(Fun, {Format, Data}) ->
  TxFun = fun() -> mnesia_transaction(Fun) end,
  trace_mnesia_transaction_restarts(TxFun, {Format, Data}).

trace_mnesia_transaction_restarts(TxFun, {Format, Data}) ->
  TracerClient = Tracee = self(),
  {ok, TraceFlags, Tracer} = setup_trace(Tracee, TracerClient),
  TxResult = TxFun(),
  ok = cleanup_trace(Tracee, TraceFlags),
  Restarts = tracer_client(Tracer, TracerClient),
  case Restarts of
    0 -> %% No contention
      %% Do not log in order to avoid excessive and irrelevant logging
      nothing;
    R when R > 0 ->
      io:format(
        user, "Tx restarted ~p times by pid ~p for " ++ Format ++ "~n",
        [R, self() | Data])
  end,
  TxResult.

tracer_server(MFA, Tracee, TracerClient) ->
  receive
    {TracerClient, restarts} ->
      Msg = {trace, Tracee, call, MFA},
      R = count_message_in_mailbox(Msg),
      Tracee ! {self(), {restarts,R}}
  end.

tracer_client(TracerServer, TracerClient) when TracerClient == self() ->
  TracerServer ! {TracerClient, restarts},
  receive
    {TracerServer, {restarts,R}} when is_integer(R) andalso R >= 0 ->
      R
  end.

setup_trace(Tracee, TracerClient) ->
  %% mnesia_tm increases the trans_restarts counter before restarting
  %% the Mnesia transaction. See mnesia_tm:restart.
  MFA = {M,F,A} = {mnesia_lib, incr_counter, [trans_restarts]},
  Tracer = spawn_link(fun() -> tracer_server(MFA, Tracee, TracerClient) end),
  TraceFlags = [call, return_to, %% Needed by erlang:trace_pattern
                {tracer, Tracer}], %% The tracer cannot be equal to the tracee
  1 = erlang:trace(Tracee, true, TraceFlags),
  1 = erlang:trace_pattern({M, F, length(A)}, [{A,[],[]}]),
  {ok, TraceFlags, Tracer}.

count_message_in_mailbox(Msg) ->
  count_message_in_mailbox(Msg, 0).
count_message_in_mailbox(Msg, Count) ->
  receive
    Msg ->
      count_message_in_mailbox(Msg, 1 + Count)
  after
    0 ->
      Count
  end.

cleanup_trace(Tracee, TraceFlags) ->
  1 = erlang:trace(Tracee, false, TraceFlags),
  ok.


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
