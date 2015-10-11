-module(ice_pp).

-export([pretty_print/1]).

%% API

pretty_print (AST) ->
    lists:flatten(pp(AST, 0)).

%% Internals

s (N) -> string:chars($ , 2 * N).   %% Prepend spaces (Tab width * N).
%% es (T) -> [S] = io_lib:format("~p",[T]), S.   %% Ensure T is string().

pp({var_decl, _, {id, Name}, Body}, N) ->
	["var ", Name, " =\n", s(N+1), pp(Body, N+2),"\n"];

pp({dim_decl, _, {id, Name}, Body}, N) ->
	["dim ", Name, " <- ", pp(Body, N),"\n"];

pp({fun_decl, _, {id, Name}, Params,Body}, N) ->
	["fun ", Name, 
		[[".", pp(BP, N)] || {b_param, BP} <- Params],
		[[" ", pp(NP, N)] || {n_param, NP} <- Params],
		" =\n", s(N+1), pp(Body, N+2),"\n"];

pp({int, V}, _) -> integer_to_list(V);
pp({float, V}, _) -> io_lib:format("~ff0", [V]);
pp({char, V}, _) -> io_lib:format("'~s'", [[V]]);
pp({string, V}, _) -> io_lib:format("\"~s\"", [V]);
pp({bool, V}, _) -> io_lib:format("~s", [V]);

pp({id, V}, _) -> io_lib:format("~s", [V]);
pp({'#', X}, N) -> 
	["#.", pp(X, N)];

pp({primop, O, [A, B]}, N) ->
	["(",pp(A, N),  io_lib:format(" ~s ", [O]), pp(B,N), ")"];
pp({primop, O, Es}, N) ->
	[io_lib:format(" ~s(", [O]), [[" ", pp(E, N)] || E <- Es] , " ) "];

pp({t, Es}, N) ->
	["[", string:join([lists:flatten([pp(Dim, N), " <- ", pp(E, N)]) || {Dim, E} <- Es], ", "), "]"];

pp({'@', A, B}, N) ->
	[pp(A, N), " @ ", pp(B,N)];

pp({'if', A, B, C}, N) ->
	["if ", pp(A, N),
	 "\n", s(N+1), "then\n",
	s(N+2), pp(B, N+2),
	 "\n", s(N+1), "else\n",
	s(N+2), pp(C, N+2),
	"\n", s(N), "fi"];

pp({fn_call, Name, Params}, N) ->
	["(", pp(Name, N), 
		[[".", pp(BP, N)] || {b_param, BP} <- Params],
		[[" ", pp(NP, N)] || {n_param, NP} <- Params],
	")"];

pp(X, _N) ->
	io_lib:format("~p", [X]).


