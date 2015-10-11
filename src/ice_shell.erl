-module(ice_shell).
-export([start/0, main/1]).

-record(state, {prompt= "",
                prompt2 = "",
                defs = [],
                tstamp = os:timestamp()}).

start() ->
    Args = init:get_plain_arguments(),
    main(Args).

main(Args) ->
    {Lines, Files} = arg_parse(Args, [], []),
    S0 = load_files(Files, #state{}),
    error_logger:tty(false),
    new_cache(),
    case Lines of
        [] ->
            interactive(S0);
        _ ->
            do_script(Lines, S0)
    end,
    init:stop().

arg_parse([], Inputs, Files) ->
    {lists:reverse(Inputs), lists:reverse(Files)};
arg_parse(["-l", File | Rest], Is, Fs) ->
    arg_parse(Rest, Is, [File | Fs]);
arg_parse(["-e", Input | Rest], Is, Fs) ->
    arg_parse(Rest, [Input ++ "\n" | Is], Fs);
arg_parse([Input | Rest], Is, Fs) ->
    NewIs = cons_file_lines(Input, Is),
    arg_parse(Rest, NewIs, Fs).

cons_file_lines(File, Acc0) ->
    {ok, Bin} = file:read_file(File),
    lists:foldl(fun(L, A) -> [L ++ "\n" | A] end, Acc0,
                string:tokens(binary_to_list(Bin), "\n")).

load_files(Files, S0) ->
    lists:foldl(fun(F, #state{defs = Ds} = S) ->
                        S#state{defs = load(F, Ds)}
                end, S0, Files).

do_script([], S) ->
    {quit, S};
do_script([L | Lines], S0) ->
    case do_line(L, S0) of
        {need_more, L2, S1} ->
            [Next | Rest] = Lines,
            do_script([L2 ++ "\n" ++ Next | Rest], S1);
        S2 ->
            do_script(Lines, S2)
    end.
		
interactive(S0) ->
    {Prompt, P2} = case io:columns() of
                       {ok, _} ->
                           {"ICE> ", "ICE? "};
                       {error, enotsup} ->
                           {"", ""}
                   end,
    loop(S0#state{prompt = Prompt, prompt2 = P2, tstamp = erlang:now()}).

loop({quit, S}) ->
	{quit, S};
loop({need_more, L0, #state{} = S}) ->
    S1 = do_cont(L0, S),
    loop(S1);
loop(#state{prompt = P} = S) ->
    case io:get_line(P) of
        eof ->
            {quit, S};
        {error, _} ->
            {quit, S};
        Line ->
            S2 = do_line(Line, S),
            loop(S2)
    end.

do_cont(L, #state{prompt2 = P} = S) -> %% FIXME!
    case io:get_line(P) of
        eof -> {quit, S};
        {error, _} -> {quit, S};
        "\n" -> S;
        Line ->
            do_line(L ++ Line, S)
    end.

do_line(L, #state{defs = Defs, tstamp = TStamp} = S) ->
    try check_line(L) of
        empty ->
            S;
        {def, {Name, Type, _} = D} ->
            if Type =:= var -> new_cache(); true -> ok end,
            NewDefs = lists:keystore(Name, 1, Defs, D),
            S#state{defs = NewDefs};
        {query, Q} ->
             do_query(Q, S, no_tc);
        {command, d} ->
            S#state{defs = []};
        {command, q} ->
            {quit, S};
        {command, p} ->
            print(Defs),
            S;
        {command, time} ->
            Now = erlang:now(),
            io:format("~fs\n", [timer:now_diff(Now, TStamp)/1000/1000]),
            S#state{tstamp = Now};
        {command, {time_expr, Expr}} ->
            QQ = parse_line(Expr),
             do_query(QQ, S, tc);
        {command, {l, Name}} ->
            new_cache(),
            S#state{defs = load(Name, Defs)};
         {badcommand, BadC} ->
            io:format("*** Bad command: `~s'\n", [BadC]),
            S
    catch
	    throw:{parse_error, {error, {_, _, ["syntax error before: ",[]]}}} ->
        	{need_more, L, S};
	    throw:{parse_error, {error, {_, _, Desc}}} ->
		    io:format("*** ~s\n", [Desc]),
		    S
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
        {match, ["t", ""]} ->
            {command, time};
        {match, ["t", Expr]} ->
            {command, {time_expr, Expr}};
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
            {Name, Type, Def} = get_name(parse_line(S)),
            {def, {Name, Type, Def}};
        nomatch ->
            {query, parse_line(S)}
    end.

parse_line(S) ->
	{ok, [X]} = ice_parser:string(S),
	X.

load(Name, OrigDefs) ->
    try
        {ok, AST0} = ice_parser:file(Name),
        lists:foldl(fun (D, Defs) -> lists:keystore(Name, 1, Defs, D) end,
                    OrigDefs,
                    [get_name(D) || D <- AST0])
    catch
        _:E ->
            io:format("*** error while loading  `~s': ~p\n", [Name, E]),
            OrigDefs
    end.

get_name({declaration, _,
          {dim_decl, _, {id, _, Name}, _Body}= Decl}) ->
    {Name, dim, Decl};
get_name({declaration, _,
          {var_decl, _, {id, _, Name}, _Body} = Decl}) ->
    {Name, var, Decl};
get_name({declaration, _,
          {fun_decl, _, {id, _, Name}, _Params, _Body} = Decl}) ->
    {Name, var, Decl};
get_name(X) ->
    throw({nodecl, expr, X}).

expr_where_defs(Expr, Defs) ->
    Dims = [Def || {_Name, dim, Def} <- Defs],
    Vars = [Def || {_Name, var, Def} <- Defs ],
    ice_ast:transform([{expr, 0, {where, 0, Expr, Dims, Vars}}]).

print(Defs) ->
    [io:format("~s\n", [ice_pp:pretty_print(ice_ast:transform([Def]))]) || {_Name, _T, Def} <- Defs],
    ok.

do_query(Q, #state{defs = Defs} = S, TC) ->
    try
        QQ = expr_where_defs(Q, Defs),
        case TC of
            no_tc ->
                {Res, _} = ice:eval(QQ),
                io:format("~p\n", [Res]);
            tc ->
                {US, {R, _}} = timer:tc(ice, eval, [QQ]),
                io:format("(~fs) ~p\n", [US/1000/1000, R])
        end,
        S
    catch
        Class:Err ->
            io:format("*** ~p:~p in ~p\n~p\n",
                      [Class, Err, Q, erlang:get_stacktrace()]),
            new_cache(),
            S
    end.

new_cache() ->
    try ice_cache:delete() of
        true ->
            ice_cache:create()
    catch 
          exit:{noproc, _} ->
              ice_cache:create();
         _:{badmatch, {aborted, _}} -> %% Mnesia
            ice_cache:create()
    end.
