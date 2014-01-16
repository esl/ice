-module(ice_file).

-export([parse/1]).

-spec parse(string()) -> {ok | error, term()}.
parse(Filename) ->
  {ok, Tree} = ice_parser:file(Filename),
  ice_ast:transform(Tree).

