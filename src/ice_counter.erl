%%------------------------------------------------------------------------------
%% Generate Unique Integers
%%------------------------------------------------------------------------------
-module(ice_counter).

-export([start_link/0, stop/0]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, terminate/2, code_change/3]).

-export([inc/0]).

-record(state, { n = 0 }).

%%==============================================================================
%% External Definitions
%%==============================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, shutdown).

inc() ->
  gen_server:call(?MODULE, {inc, []}).

%%==============================================================================
%% Internal Definitions
%%==============================================================================

handle_call({inc, _}, _From, S) ->
  {reply, S#state.n, S#state{ n = S#state.n + 1 }}.

handle_cast(shutdown, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, Srv) ->
  {noreply, Srv}.

init([]) ->
  {ok, #state{}}.
  
terminate(_Reason, _Srv) ->
  ok.

code_change(_OldVsn, _Srv, _Ext) ->
  {ok, _Srv}.
