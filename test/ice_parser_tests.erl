%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_parser_tests).

%% ice_parser_tests: tests for module ice_parser, inspired from ice_parse_tests.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

module_decl_test() ->
  Case1 = "module m1",
  Case2 = "module m1 where import m2 end",
  Case3 = "module m1 where import m2 as m end",
  Case4 = "module m1 where import m2 (x, y) end",
  Case5 = "module m1 where export x end",
  Case6 = "module m1 where export (x, y) end",

  ?assertMatch({ok,[{declaration,_,{module,_,{id,_,"m1"}}}]}, 
	       ice_parser:string(Case1)),
  ?assertMatch({ok,[{declaration,_,{module,_,{id,_,"m1"},[{import,_,{id,_,"m2"}}]}}]}, 
	       ice_parser:string(Case2)),
  ?assertMatch({ok,[{declaration,_,{module,_,{id,_,"m1"},[{import_as,_,{id,_,"m2"},{id,_,"m"}}]}}]}, 
	       ice_parser:string(Case3)),  
  ?assertMatch({ok,[{declaration,_,{module,_,{id,_,"m1"},[{import_only,_,{id,_,"m2"},[{id,_,"x"},{id,_,"y"}]}]}}]}, 
	       ice_parser:string(Case4)),
  ?assertMatch({ok,[{declaration,_,{module,_,{id,_,"m1"},[{export,_,{id,_,"x"}}]}}]}, 
	       ice_parser:string(Case5)),
  ?assertMatch({ok,[{declaration,_,{module,_,{id,_,"m1"},[{export_all,_,[{id,_,"x"},{id,_,"y"}]}]}}]}, 
	       ice_parser:string(Case6)).

var_decl_test () ->
    ?assertMatch({ok,[{declaration,_,{var_decl,_,{id,_,"bar"},{int,_,42}}}]},
        ice_parser:string("var bar = 42")).

fun_decl_test () ->
    ?assertMatch({ok,[{declaration,_,{fun_decl,_,{id,_,"f"},[
            {named_param,_,{id,_,"a"}},
            {base_param,_,{id,_,"b"}},
            {base_param,_,{id,_,"die"}},
            {value_param,_,{id,_,"antwoord"}}],{int,_,42}}}]},
        ice_parser:string("fun f a.b.die!antwoord = 42")).

where_end_test () ->
    ?assertMatch({ok,[{declaration,_,{dim_decl,_,{id,_,"m"},
            {'where',_,{id,_,"n"},[],[{var_decl,_,{id,_,"n"},{int,_,42}}]}
        }}]},
        ice_parser:string("dim m <- n where var n = 42 end")).

where_end__hard_test () ->
    ?assertMatch({ok,[{declaration,_,{dim_decl,_,_,{
        where,_,{id,_,"n"},
            [{dim_decl,_,{id,_,"m"}}],
            [{var_decl,_,{id,_,"n"},_},
             {var_decl,_,{id,_,"j"},_},
             {fun_decl,_,{id,_,"f"},_,_}]}}}]},
        ice_parser:string(
            "dim m <- n     "
            "where          "
            "   var n = j   "
            "   var j = f   "
            "   fun f.a = a "
            "   dim m       "
            "end            ")).

application_simple_test () ->
    ?assertMatch({ok,[{expr,_,{call,_,{id,_,"fact"},[{base_param,_,{int,_,500}}]}}]},
        ice_parser:string("fact.500")).

application_hard_test () ->
    ?assertMatch({ok,[{expr,_,
            {call,_,
                  {id,_,"fact"},
                  [ {named_param,_,{int,_,500}}
                  , {named_param,_,{int,_,42}}
                  , {base_param,_,{id,_,"arg3"}}
                  , {base_param,_,{id,_,"Y"}}]}}]},
        ice_parser:string("fact 500 42.arg3.Y")).

application_weird_test () ->
    ?assertMatch({ok,[{declaration,_,{var_decl,_,{id,_,"m"},
            {call,_,{id,_,"print"},[{value_param,_,{int,_,3}},
                                    {base_param,_,{raw_string,_,"Error!"}}]}}}]},
        ice_parser:string("var m = print!3.`Error!`")).

big_priority_test () ->
    ?assertMatch({ok,[{expr,_,
            {'or',_,
                {'and',_,
                    {'==',_,
                        {'+',_,
                            {'*',_,
                                {'..',_,
                                    {'#!',_,{'+',_,{int,_,42}}},
                                {id,_,"g"}},
                            {id,_,"f"}},
                        {id,_,"e"}},
                    {'@',_,{id,_,"d"},{id,_,"c"}}},
                {id,_,"b"}},
            {id,_,"a"}}}]},
        ice_parser:string("#! (+02a) .. g * f + e == d @ c and b || a")).

range_priority_test_ () ->
    Loc = 1,
    Range = {'..',Loc,{int,Loc,1},{id,Loc,"n"}},
    Two   = {int,Loc,2},
    [ ?_assertMatch({ok,[
        {declaration,_,
            {fun_decl,_,
                {id,_,"f"},
                [{named_param,_,{id,_,"n"}}],
                {'-',_,
                    Range,
                    Two}}}]},
        ice_parser:string("fun f n = 1..n-2"))
    , ?_assertMatch({ok,[
        {declaration,_,
            {fun_decl,_,
                {id,_,"f"},
                [{named_param,_,{id,_,"n"}}],
                {'-',_,
                    Two,
                    Range}}}]},
        ice_parser:string("fun f n = 2-1..n")) ].

-define(id(X), {expr,_,{id,_,X}}).

-define(bool(X), {expr,_,{bool,_,X}}).
bool_test () ->
    ?assertMatch({ok,[?bool(true),?bool(false),?id("True")]},
        ice_parser:string("true ;; false ;; True")).

-define(int(X), {expr,_,{int,_,X}}).
int_test () ->
    ?assertMatch({ok,[{expr,_,{'-',_,{int,_,42}}},?int(42),?int(162)]},
        ice_parser:string("-02a ;; 42 ;; 0a2 //;; 0zz\n")).

-define(float(X), {expr,_,{float,_,X}}).
float_test () ->
    ?assertMatch({ok,[{expr,_,{'-',_,{float,_,42.0}}},?float(42.0),?float(42.1e3),?float(13.37E3)]},
        ice_parser:string("-42.0f0 ;; 42.0f0 ;; 42.1f3 ;; 13.37f3")).

-define(char(X), {expr,_,{char,_,X}}).
char_test () ->
    ?assertMatch({ok,[?char($a),?char($b),?char($2)]},
        ice_parser:string("'a' ;; 'b' ;; '2'")).

-define(raw_string(X), {expr,_,{raw_string,_,X}}).
raw_string_test () ->
    ?assertMatch({ok,[?raw_string("This "),?raw_string("!Z"),
                       ?raw_string(" a ra\\/\\/ "),?raw_string("strin()")]},
        ice_parser:string("`This ` ;; `!Z` ;; ` a ra\\/\\/ ` ;; `strin()`")).

-define(cooked_string(X), {expr,_,{cooked_string,_,X}}).
cooked_string_test () ->
    ?assertMatch({ok,[?cooked_string("Ook"),?cooked_string("e\td "),
                       ?cooked_string("sTr!N{}")]},
        ice_parser:string("\"Ook\" ;; \"e\\td \" ;; \"sTr!N{}\" ")).

tuple_test () ->
    ?assertMatch({ok,[{expr,_,{tuple,_,[{tuple_element,_,{id,_,"n"},{int,_,42}},
                                         {tuple_element,_,{id,_,"m"},{int,_,24}}]}}]},
        ice_parser:string(" [n <- 42, m <- 24] ")).

prefixop_test () ->
    ?assertMatch({ok,[{expr,_,{'-',_,{id,_,"e"},{'+',_,{int,_,3}}}}]},
        ice_parser:string("e - (+3)")).

colons_everywhere_test () ->
    ?assertMatch({ok,[{expr,_,
        {where,_,
                 {call,_,{'#.',_,{id,_,"d"}},[{base_param,_,{int,_,46}}]},
                 [{dim_decl,_,{id,_,"d"},
                            {where,_,
                                {id,_,"F"},
                                [],
                                [{fun_decl,_,
                                    {id,_,"F"},
                                    [{base_param,_,{id,_,"b"}}],
                                    {id,_,"b"}}]}}],
                 []}}]},
        ice_parser:string("(#.d).46 where dim d <- F where fun F.b = b;; end;; end")).

correct_top_expr_where_end_is_correct_test () ->
    ?assertMatch({ok,[{expr,_,
       {where,_,
              {call,_,{id,_,"Sum"},[{base_param,_,{float,_,1.2}}]},
              [],
              [{fun_decl,_,
                         {id,_,"Sum"},
                         [{base_param,_,{id,_,"a"}},{base_param,_,{id,_,"b"}}],
                         {'+',_,{id,_,"a"},{id,_,"b"}}}]}}]},
        ice_parser:string("(Sum.1.2f0) where fun Sum.a.b = a+b end")).

no_need_for_parenthesis_in_where_end_top_expr_test () ->
    ?assertMatch({ok,[{expr,_,
       {where,_,
              {call,_,
                    {'#.',_,{id,_,"d"}},
                    [{named_param,_,{int,_,46}},{base_param,_,{id,_,"e"}}]},
              [],
              [{var_decl,_,{id,_,"e"},{int,_,0}}]}}]},
        ice_parser:string("(#.d) 46.e where var e = 0 end")).

rework_call_of_primop_with_function_call_test () ->
    ?assertMatch({ok,[{declaration,_,
        {fun_decl,_,
            {id,_,"f"},
            [{base_param,_,{id,_,"n"}}],
            {'/',_,
                {id,_,"n"},
                {call,_,
                    {id,_,"f"},
                    [{named_param,_,{'-',_,{id,_,"n"},{int,_,1}}},
                     {base_param,_,{id,_,"m"}},
                     {value_param,_,{id,_,"jjj"}}]}}}}]},
        ice_parser:string("fun f.n = n / f (n-1).m!jjj")).

funcall_with_args_is_high_priority_test () ->
    ?assertMatch({ok,[{expr,_,
           {where,_,
                  {call,_,{id,_,"f"},[{base_param,_,{int,_,3}}]},
                  [],
                  [{fun_decl,_,
                             {id,_,"f"},
                             [{base_param,_,{id,_,"n"}},
                              {named_param,_,{id,_,"X"}},
                              {named_param,_,{id,_,"Y"}}],
                             {'*',_,
                                  {call,_,
                                        {id,_,"f"},
                                        [{base_param,_,{'-',_,{id,_,"n"},{int,_,1}}},
                                         {named_param,_,{id,_,"X"}},
                                         {named_param,_,{id,_,"Y"}}]},
                                  {id,_,"n"}}}]}}]},
        ice_parser:string(" f.3 where fun f.n X Y = f.(n-1) X Y * n end ")).

intension_creation_by_the_book_test () ->
    ?assertMatch({ok,[{expr,_,
           {'@',_,
                {intension_evaluation,_,{id,_,"tempInuvik"}},
                {tuple,_,
                       [{tuple_element,_,
                                       {id,_,"location"},
                                       {cooked_string,_,"Paris"}},
                        {tuple_element,_,
                                       {id,_,"date"},
                                       {'-',_,{'#.',_,{id,_,"date"}},{int,_,1}}}]}}}]},
        ice_parser:string(" (↓ tempInuvik ) @ [location ← \"Paris\", date <- #.date - 1] ")).

intensions_in_short_test () ->
    ?assertMatch({ok,[{expr,_,
            {intension_evaluation,_,
                {intension_creation,_,
                    [],
                    {int,_,46}}}}]},
        ice_parser:string(" ↓(↑{} 46);; ")).

intensions_with_one_frozen_dim_test () ->
    ?assertMatch({ok,[{expr,_,
            {intension_evaluation,_,
                {intension_creation,_,
                    [{id,_,"t"}],
                    {int,_,46}}}}]},
        ice_parser:string(" ↓(↑{t} 46);; ")).

intensions_with_multiple_frozen_dim_test () ->
    ?assertMatch({ok,[{expr,_,
            {intension_evaluation,_,
                {intension_creation,_,
                    [{id,_,"t"},{id,_,"s"}],
                    {int,_,46}}}}]},
        ice_parser:string(" ↓(↑{t,s} 46);; ")).

nested_lambdas_with_multiple_arguments_FTW_test () ->
    ?assertMatch({ok,[{expr,_,
           {lambda,_,[],
                   [{named_param,_,{id,_,"a"}},
                    {base_param, _,{id,_,"b"}},
                    {value_param,_,{id,_,"c"}},
                    {named_param,_,{id,_,"X"}},
                    {named_param,_,{id,_,"Y"}}],
                   {lambda,_,[],
                           [{named_param,_,{id,_,"stmt"}}],
                           {id,_,"stmt"}}}}]},
        ice_parser:string(" \\a.b!c X Y -> λstmt -> stmt ")).

extension_declaration_test () ->
    ?assertMatch({ok,[{declaration,_,
            {ext_decl,_
            , {id,_,"a"}
            , [ {ext_ty,_,{id,_,"s"},{cl_scalar,_,"float"}} ]
            , {cl_scalar,_,"int"}
            , {'+',_,
                    {'#.',_,{id,_,"s"}},
                    {int,_,42}}
            , {int,_,1}}}]},
        ice_parser:string("ext 1 a :: (s::float) -> int = #.s + 42")).

extension_declapp_test () ->
    ?assertMatch({ok,[{expr,_,
            {where,_,
                {id,_,"B"},
                [{dim_decl,_,{id,_,"t"},{int,_,0}}],
                [
                    {var_decl,_,
                        {id,_,"B"},
                        {'+',_,
                            {'@',_,
                                {id,_,"A"},
                                {tuple,_,[{tuple_element,_,{id,_,"t"},{int,_,0}}]}},
                            {'@',_,
                                {id,_,"A"},
                                {tuple,_,[{tuple_element,_,{id,_,"t"},{int,_,1}}]}}}},
                    {ext_decl,_,
                        {id,_,"A"},
                        [ {ext_ty,_,{id,_,"t"},{cl_scalar,_,"uint"}}
                        , {ext_ty,_,{id,_,"s"},{cl_scalar,_,"float"}} ],
                        {cl_scalar,_,"int"},
                        {'+',_,
                            {'+',_,
                                {'#.',_,{id,_,"t"}},
                                {'#.',_,{id,_,"s"}}},
                            {int,_,42}},
                        {int,_,512}}]}}]},
        ice_parser:string("B"                                          "\n"
                       "where"                                         "\n"
                       "  var B = A @ [t <- 0]"                        "\n"
                       "        + A @ [t <- 1]"                        "\n"
                       "     // ..."                                   "\n"
                       "  ext 512"                                     "\n"
                       "      A :: (t::uint s::float) -> int"          "\n"
                       "    = #.t + #.s + 42"                          "\n"
                       "  dim t <- 0"                                  "\n"
                       "end")).

unary_not_test_ () ->
    [ ?_assertMatch({ok,[{expr,_,
           {'and',_,{bool,_,true},{'not',_,{bool,_,false}}}}]},
           ice_parser:string("true and not false"))
    , ?_assertMatch({ok,[{expr,_,
           {'and',_,{'not',_,{bool,_,true}},{bool,_,false}}}]},
           ice_parser:string("not true and false")) ].

pguard_test () ->
    ?assertMatch({ok,[{declaration,_,
                  {fun_decl,_,
                            {id,_,"plus"},
                            [{value_param,_,{id,_,"a"}},{value_param,_,{id,_,"b"}}],
                            {'+',_,{id,_,"a"},{id,_,"b"}},
                            [],
                            {'and',_,
                                   {'==',_,
                                         {call,_,{id,_,"a"},[{base_param,_,{id,_,"prec"}}]},
                                         {call,_,{id,_,"b"},[{base_param,_,{id,_,"prec"}}]}},
                                   {'==',_,
                                         {call,_,{id,_,"a"},[{base_param,_,{id,_,"is_signed"}}]},
                                         {call,_,
                                               {id,_,"b"},
                                               [{base_param,_,{id,_,"is_signed"}}]}}}}}]},
        ice_parser:string("fun plus!a!b  | a.prec == b.prec && a.is_signed == b.is_signed = a + b")).

tguard_test () ->
    ?assertMatch({ok,[{declaration,_,
                  {fun_decl,_,
                            {id,_,"nuf"},
                            [{base_param,_,{id,_,"a"}},{value_param,_,{id,_,"b"}}],
                            {id,_,"a"},
                            [{tguard,_,{id,_,"a"},{'..',_,{int,_,1},{int,_,2}}},
                             {tguard,_,{id,_,"b"},{int,_,0}}]}}]},
        ice_parser:string("fun nuf.a!b [a : 1..2, b:0] = a")).

both_guards_test () ->
    ?assertMatch({ok,[{declaration,_,
                  {fun_decl,_,
                            {id,_,"f"},
                            [{base_param,_,{id,_,"n"}},{base_param,_,{id,_,"m"}}],
                            {int,_,1},
                            [{tguard,_,{id,_,"n"},{int,_,0}},
                             {tguard,_,{id,_,"m"},{'..',_,{int,_,1},{id,_,"k"}}}],
                            {'not',_,
                                   {call,_,{id,_,"is_float"},[{base_param,_,{id,_,"m"}}]}}}}]},
        ice_parser:string("fun f.n.m [n : 0, m : 1..k] | not is_float.m = 1")).

binder_test () ->
    ?assertMatch({ok,[{expr,_,
                  {'if',_,
                        [{if_expr,_
                         , {id,_,"X"}
                         , {';',_
                           , {id,_,"Y"}
                           , {call,_,{id,_,"YY"},[{named_param,_,{int,_,2}}]}}}]
                  , {id,_,"Z"}}}]},
        ice_parser:string(" if X then Y ; YY 2 else Z fi ")).

%% Internals

%% End of Module.
