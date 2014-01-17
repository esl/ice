-module(ice_string).

-export([parse/1]).

-spec parse(string()) -> {ok | error, term()}.
parse(IceCode) ->
  {ok, Tree} = ice_parser:string(IceCode),
  ice_ast:transform(Tree).
