-module(ice_shell).
-export([start/0]).

-record(state, {prompt= "", cache = clear, defs = []}).

start() ->
    error_logger:tty(false),
    Prompt = case io:columns() of
       {ok, _} -> "ICE> ";
       {error, enotsup} -> ""
    end,
    loop(#state{prompt = Prompt}).

loop(#state{prompt = P} = S) ->
    case io:get_line(P) of
        eof -> init:stop();
        {error, _} -> init:stop();
        Line ->
            do_line(Line, S)
    end.

do_line(L, #state{defs = Defs, cache = C} = S) ->
    case check_line(L) of
        empty ->
            loop(S);
        {def, {Name, _} = D} ->
            NewDefs = lists:keystore(Name, 1, Defs, D),
            loop(#state{defs = NewDefs});
        {query, Q} ->
            Query = Q ++ where_defs(Defs),
            try
		{Res, _} = eval(Query, C),
                io:format("~p\n", [Res]),
                loop(S)
            catch Class:Err ->
                io:format("*** ~p:~p in ~s\n~p\n", [Class,Err, Query, erlang:get_stacktrace()]),
                catch ice_cache:delete(),
                loop(S#state{cache = clear})
            end;
        {command, d} ->
            loop(S#state{defs = []});
        {command, q} ->
            init:stop();
        {command, p} ->
            print(Defs),
            loop(S);
        {command, {l, Name}} ->
            loop(S#state{defs = load(Name, Defs)});
        {command, cache_on} ->
            ice_cache:create(),
            loop(S#state{cache = keep});
        {command, cache_off} ->
             ice_cache:delete(),
            loop(S#state{cache = clear});
        {command, print_cache_state} ->
            io:format("***cache ~p\n", [C]),
            loop(S);
         {badcommand, C} ->
            io:format("*** Bad command: `~s'\n", [C]),
            loop(S)
    end.

check_line(S) ->
    case re:run(S, "^\\s*(//.*)?$") of
        nomatch ->
            check_command(S);
        _ ->
            empty
    end.

check_command(S) ->
    case re:run(S, "^:(\\w+)\\s+(.*)", [{capture, [1,2], list}]) of
        {match, ["d", ""]} ->
            {command, d};
        {match, ["p", ""]} ->
            {command, p};
        {match, ["q", ""]} ->
            {command, q};
        {match, ["c", OnOff]} ->
            case string:strip(OnOff) of
                "on"++ _ ->
                    {command, cache_on};
                 "off" ++ _ ->
                     {command, cache_off};
                 _ ->
                     {command, print_cache_state}
             end;
        {match, ["l", File]} ->
            {command, {l, File}};
        {match, [C|_]} ->
            {badcommand, C};
        nomatch ->
            check_def(S)
    end.

check_def(S) ->
    case re:run(S, "^\\s*(fun|var|dim)\\s+(\\w+)", [{capture, [2],list}]) of
        {match, [Name]} ->
            {def, {Name, S}};
        nomatch ->
            {query, S}
    end.

load(Name, OrigDefs) ->
    try
        {ok, Bin} = file:read_file(Name),
        lists:foldl(fun do_file_line/2,
                    OrigDefs,
                    [S ++ "\n" || S <- string:tokens(binary_to_list(Bin), "\n")])
    catch _ : E ->
            io:format("*** error while loading  `~s': ~p\n", [Name, E]),
            OrigDefs
    end.

do_file_line(L, Defs) ->
    case check_line(L) of
        empty ->
            Defs;
        {def, {Name, _} = D} ->
            lists:keystore(Name, 1, Defs, D);
        Other ->
            throw(Other)
    end.

where_defs(Defs) ->
    XDefs = [Def || {_Name, Def} <- Defs],
    lists:flatten(["  where\n\t", string:join(XDefs, "\t"), "  end\n"]).

print(Defs) ->
    [io:format("~s", [Def]) || {_Name, Def} <- Defs],
    ok.

eval(String, C) ->
    Tree = ice_string:parse(String),
    case C of
              clear ->
                  ice_cache:create(),
                  Res = ice:eval(Tree),
                  ice_cache:delete(),
                  Res;
              keep ->
                  ice:eval(Tree)
              end.
