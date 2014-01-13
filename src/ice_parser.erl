%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_parser).

%% ice_parser: wrapper around the generated lexer and parser.

-export([string/1,file/1]).
-export([scan/1,parse/1]).
% TODO: error reporting. See EXPORTS from Leex & Yecc doc pages.

-ifdef(DEBUG).
- define(say(S,L), io:format(S,L)).
-else.
- define(say(S,L), ok).
-endif.

%% API

scan (String) ->
    ice_scan:string(String).

parse (Tokens) ->
    ice_parse:parse(Tokens).

file (Filename) ->
    {ok, Dev} = file:open(Filename, [read,unicode]),
    String = assemble_lines(Dev, []),
    string (String).

string (String) ->
    case scan(String) of
        {ok, Tokens, _Loc} ->
            case parse(Tokens) of
                {ok, Trees} ->
                    ProperTree = [v_atomics(Tree) || Tree <- Trees],
                    {ok, ProperTree};
                ParseError ->
                    throw ({parse_error, ParseError})
            end;

        {error, SyntaxError, _Loc} ->
            {Line, _Lexer, Descr} = SyntaxError,
            [Msg, L] = ice_scan:format_error(Descr),
            ?say ("SyntaxError on line ~p:\n\t~s~p\n", [Line,Msg,L]),
            {syntax_error, Line, Msg, L}
    end.


%% Internals

assemble_lines (Dev, Acc) ->
    case file:read_line(Dev) of
        {ok, Line} ->
            assemble_lines(Dev, [Line|Acc]);
        eof ->
            ok = file:close(Dev),
            lists:flatten(lists:reverse(Acc))
    end.

%% Visits the AST to replace atomic values.
v_atomics (Tree) ->
    ice_visitor:visit(
        fun ({bool,          L, S}) -> {bool,          L, treat_bool(S)};
            ({int,           L, S}) -> {int,           L, treat_int(S)};
            ({float,         L, S}) -> {float,         L, treat_float(S)};
            ({char,          L, S}) -> {char,          L, treat_char(S)};
            ({raw_string,    L, S}) -> {raw_string,    L, treat_raw_string(S)};
            ({cooked_string, L, S}) -> {cooked_string, L, treat_cooked_string(S)}
        end,
    Tree, top_down).

treat_bool ("true") -> true;
treat_bool ("false") -> false.

treat_int (Int) ->
    case re:run(Int, "^[0-9]+$") of
        {match, _} -> list_to_integer(Int);
        nomatch ->
            case re:run(Int, "^0[0-9A-Fa-f]+$") of
                {match, _} -> list_to_integer(Int, 16)  %TODO: handle the base properly.
            end
    end.

treat_float (Float) ->
    list_to_float([case C of $f -> $e; _ -> C end || C <- Float]).

treat_char ([$', Char, $']) -> Char.

strip_quotes ([_Quote, _unQuote]) -> "";
strip_quotes (Chars) -> string:substr(Chars, 2, length(Chars) - 2).

treat_raw_string (String) -> strip_quotes(String).

treat_cooked_string (String) ->
    escape(
      strip_quotes(
        String)).

escape ([$\\,C|Rest]) ->
    E = case C of
            $b -> $\b;
            $d -> $\b;
            $e -> $\e;
            $f -> $\f;
            $n -> $\n;
            $r -> $\r;
            $s -> $\s;
            $t -> $\t;
            $v -> $\v;
            C -> C
        end,
    [E|escape(Rest)];
escape ([C|Rest]) ->
    [C|escape(Rest)];
escape ([]) ->
    [].



%% End of Module.
