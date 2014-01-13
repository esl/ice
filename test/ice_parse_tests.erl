%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_parse_tests).

%% ice_parse_tests: tests for module ice_parse.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

blocks_test () ->
    {ok,[Dim_N]} = sp("dim n"),
    ?assertEqual ({ok,[Dim_N,Dim_N,Dim_N]}, sp("dim n dim n dim n")).

block_test_ () ->
    [ ?_assertMatch({ok,[{declaration,_,_}]}, sp("dim n"))
    , ?_assertMatch({ok,[{declaration,_,_}]}, sp("var v = 42"))
    , ?_assertMatch({ok,[{declaration,_,_}]}, sp("fun fact.is = ok"))
    , ?_assertMatch({ok,[{expr,_,_}]}, sp("42"))
    , ?_assertMatch({ok,[{expr,_,{call,_,_,_}}]}, sp("fact.42")) ].

dim_decl_test_ () ->
    [ ?_assertMatch({ok,[{declaration,_,{dim_decl,_,{id,_,"n"}}}]}, sp("dim n"))
    , ?_assertMatch({ok,[{declaration,_,{dim_decl,_,{id,_,"n"},{int,_,"0"}}}]}, sp("dim n <- 0")) ].

var_decl_test () ->
    ?assertMatch({ok,[{declaration,_,{var_decl,_,{id,_,"bar"},{int,_,"42"}}}]},
        sp("var bar = 42")).

fun_decl_test () ->
    ?assertMatch({ok,[{declaration,_,{fun_decl,_,{id,_,"f"},
                                       [ {named_param,_,{id,_,"a"}}
                                       , {base_param,_,{id,_,"b"}}
                                       , {base_param,_,{id,_,"die"}}
                                       , {value_param,_,{id,_,"antwoord"}}
                                       ],{int,_,"42"}}}]},
        sp("fun f a.b.die!antwoord = 42")).

where_end_test () ->
    ?assertMatch({ok,[{declaration,_,{dim_decl,_,{id,_,"m"},
            {'where',_,{id,_,"n"},[],[{var_decl,_,{id,_,"n"},{int,_,"42"}}]}
        }}]},
        sp("dim m <- n where var n = 42 end")).

where_end__hard_test () ->
    ?assertMatch({ok,[{declaration,_,{dim_decl,_,_,{
        where,_,{id,_,"n"},
            [{dim_decl,_,{id,_,"m"}}],
            [{var_decl,_,{id,_,"n"},_},
             {var_decl,_,{id,_,"j"},_},
             {fun_decl,_,{id,_,"f"},_,_}]}}}]},
        sp("dim m <- n     "
            "where          "
            "   var n = j   "
            "   var j = f   "
            "   fun f.a = a "
            "   dim m       "
            "end            ")).

application_simple_test () ->
    ?assertMatch({ok,[{expr,_,{call,_,
                                     {id,_,"fact"},
                                     [{base_param,_,{int,_,"500"}}]}}]},
        sp("fact.500")).

application_hard_test () ->
    ?assertMatch({ok,[{expr,_,
            {call,_,{id,_,"fact"},[ {named_param,_,{int,_,"500"}}
                                  , {named_param,_,{int,_,"42"}}
                                  , {base_param,_,{id,_,"arg3"}}
                                  , {base_param,_,{id,_,"Y"}}]}}]},
        sp("fact 500 42.arg3.Y")).

application_weird_test () ->
    ?assertMatch({ok,[{declaration,_,{var_decl,_,{id,_,"m"},
            {call,_,{id,_,"print"},[{named_param,_,{int,_,"3"}},
                                    {value_param,_,{raw_string,_,"`Error!`"}}]}}}]},
        sp("var m = print 3!`Error!`")).

big_priority_test () ->
    ?assertMatch({ok,[{expr,_,
            {'or',_,
                {'and',_,
                    {'==',_,
                        {'+',_,
                            {'*',_,
                                {'..',_,
                                    {'#!',_,{'+',_,{int,_,"02a"}}},
                                {id,_,"g"}},
                            {id,_,"f"}},
                        {id,_,"e"}},
                    {'@',1,{id,1,"d"},{id,1,"c"}}},
                {id,_,"b"}},
            {id,1,"a"}}}]},
        sp("#! (+02a) .. g * f + e == d @ c and b || a")).

range_priority_test_ () ->
    Loc = 1,
    Range = {'..',Loc,{int,Loc,"1"},{id,Loc,"n"}},
    Two   = {int,Loc,"2"},
    [ ?_assertMatch({ok,[
        {declaration,_,
            {fun_decl,_,
                {id,_,"f"},
                [{named_param,_,{id,_,"n"}}],
                {'-',_,
                    Range,
                    Two}}}]},
        sp("fun f n = 1..n-2"))
    , ?_assertMatch({ok,[
        {declaration,_,
            {fun_decl,_,
                {id,_,"f"},
                [{named_param,_,{id,_,"n"}}],
                {'-',_,
                    Two,
                    Range}}}]},
        sp("fun f n = 2-1..n")) ].

-define(id(X), {expr,_,{id,_,X}}).
id_test () ->
    ?assertMatch({ok,[?id("fvar"),?id("var1"),?id("echo"),?id("_m")]},
        sp("fvar ;; var1 ;; echo ;; _m")).

-define(bool(X), {expr,_,{bool,_,X}}).
bool_test () ->
    ?assertMatch({ok,[?bool("true"),?bool("false"),?id("True")]},
        sp("true ;; false ;; True")).

-define(int(X), {expr,_,{int,_,X}}).
int_test () ->
    ?assertMatch({ok,[{expr,_,{'-',_,{int,_,"02a"}}},?int("42"),?int("0a2"),?int("0zz")]},
        sp("-02a ;; 42 ;; 0a2 ;; 0zz")).

-define(float(X), {expr,_,{float,_,X}}).
float_test () ->
    ?assertMatch({ok,[{expr,_,{'-',_,{float,_,"42.0f0"}}},?float("42.0f0"),?float("42.1f3"),?float("13.37f3")]},
        sp("-42.0f0 ;; 42.0f0 ;; 42.1f3 ;; 13.37f3")).

-define(char(X), {expr,_,{char,_,X}}).
char_test () ->
    ?assertMatch({ok,[?char("'a'"),?char("'b'"),?char("'2'")]},
        sp("'a' ;; 'b' ;; '2'")).

-define(raw_string(X), {expr,_,{raw_string,_,X}}).
raw_string_test () ->
    ?assertMatch({ok,[?raw_string("`This `"),?raw_string("`!Z`"),
                       ?raw_string("` a ra\\/\\/ `"),?raw_string("`strin()`")]},
        sp("`This ` ;; `!Z` ;; ` a ra\\/\\/ ` ;; `strin()`")).

-define(cooked_string(X), {expr,_,{cooked_string,_,X}}).
cooked_string_test () ->
    ?assertMatch({ok,[?cooked_string("\"Ook\""),?cooked_string("\"ed \""),
                       ?cooked_string("\"sTr!N{}\"")]},
        sp("\"Ook\" ;; \"ed \" ;; \"sTr!N{}\" ")).

tuple_test () ->
    ?assertMatch({ok,[{expr,_,{tuple,_,[{tuple_element,_,{id,_,"n"},{int,_,"42"}},
                                         {tuple_element,_,{id,_,"m"},{int,_,"24"}}]}}]},
        sp(" [n <- 42, m <- 24] ")).

conditional_test () ->
    ?assertMatch({ok,[{expr,_,{'if',_,[{if_expr,_,{id,_,"this"},{id,_,"that"}},
                                        {if_expr,_,{id,_,"this"},{id,_,"that"}}],
                                    {id,_,"thus"}}}]},
        sp(" if this then that elsif this then that else thus fi")).

-define(cmpop(X), {expr,_,{X,_,{id,_,"a"},{id,_,"b"}}}).
cmpop_test () ->
    ?assertMatch({ok,[?cmpop('<'),?cmpop('<='),?cmpop('=='),?cmpop('>='),?cmpop('>'),?cmpop('!=')]},
        sp(" a < b ;; a <= b ;; a == b ;; a >= b ;; a > b ;; a != b ")).

addop_test () ->
    ?assertMatch({ok,[{expr,_,
           {'+',_,
                {'+',_,
                     {'-',_,
                          {'+',_,
                               {'+',_,
                                    {id,_,"a"},
                                    {id,_,"b"}},
                               {id,_,"c"}},
                          {id,_,"d"}},
                     {id,_,"e"}},
                {id,_,"f"}}}]},
        sp(" a + b + c - d + e + f ")).

mulop_test () ->
    ?assertMatch({ok,[{expr,_,
           {'%',_,
                {'/',_,
                     {'*',_,
                          {'*',_,
                               {id,_,"a"},
                               {id,_,"b"}},
                          {id,_,"c"}},
                     {id,_,"d"}},
                {id,_,"e"}}}]},
        sp(" a * b * c / d % e ")).

prefixop_test () ->
    ?assertMatch({ok,[{expr,_,{'-',_,{id,_,"e"},{'+',_,{int,_,"3"}}}}]},
        sp("e - (+3)")).

%% Internals

sp (String) ->
    {ok, Tokens, _Loc} = ice_scan:string(String),
    ice_parse:parse(Tokens).

%% End of Module.
