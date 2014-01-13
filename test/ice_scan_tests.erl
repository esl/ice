%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(ice_scan_tests).

%% ice_scan_tests: tests for module ice_scan.

-include_lib("eunit/include/eunit.hrl").

%% API tests.

blanks_test_ () ->
    [ ?_assertMatch({ok,[],_}, ice_scan:string(" // A comment\n "))
    , ?_assertMatch({ok,[],3}, ice_scan:string("     \t   \t\t\t \r\n \n \v ")) ].

-define(token (Atom),
    ?_assertMatch({ok,[{Atom,_}],_}, ice_scan:string(atom_to_list(Atom)))).
simple_test_ () ->
    [ ?token (';;')
    , ?token ('%%')
    , ?token ('dim')
    , ?token ('fun')
    , ?token ('var')
    , ?token ('if')
    , ?token ('then')
    , ?token ('elsif')
    , ?token ('else')
    , ?token ('fi')
    , ?token ('where')
    , ?token ('end')
    , ?token ('ext')
    , ?token ('::')
    , ?token ('<-')
    , ?token ('->')
    , ?_assertMatch({ok,[{'^|',_}],_}, ice_scan:string("↑"))
    , ?_assertMatch({ok,[{'^|',_}],_}, ice_scan:string("i^"))
    , ?_assertMatch({ok,[{'v|',_}],_}, ice_scan:string("↓"))
    , ?_assertMatch({ok,[{'v|',_}],_}, ice_scan:string("i!"))
    , ?token ('=')
    , ?token ('\\')
    , ?_assertMatch({ok,[{'\\',_}],_}, ice_scan:string("λ"))
    , ?token ('.')
    , ?token ('!')
    , ?token ('or')
    , ?_assertMatch({ok,[{'or',_}],_}, ice_scan:string("||"))
    , ?token ('and')
    , ?_assertMatch({ok,[{'and',_}],_}, ice_scan:string("&&"))
    , ?token ('@')
    , ?token ('<')
    , ?token ('<=')
    , ?token ('==')
    , ?token ('>=')
    , ?token ('>')
    , ?token ('!=')
    , ?token ('+')
    , ?token ('-')
    , ?token ('not')
    , ?token ('*')
    , ?token ('/')
    , ?token ('%')
    , ?token ('..')
    , ?token ('#.')
    , ?token ('#!')
    , ?token ('|')
    , ?token (':')
    , ?token ('(')
    , ?token ('[')
    , ?token ('{')
    , ?token (',')
    , ?token (';')
    , ?token ('}')
    , ?token (']')
    , ?token (')') ].

cl_scalar_test_ () ->
    [ ?_assertMatch({ok,[{cl_scalar,_,Ty}],_}, ice_scan:string(Ty))
    || Ty <- [   "bool",   "bool2",   "bool4",   "bool8",   "bool16"
             ,   "char",   "char2",   "char4",   "char8",   "char16"
             ,  "uchar",  "uchar2",  "uchar4",  "uchar8",  "uchar16"
             , "double", "double2", "double4", "double8", "double16"
             ,  "short",  "short2",  "short4",  "short8",  "short16"
             , "ushort", "ushort2", "ushort4", "ushort8", "ushort16"
             ,    "int",    "int2",    "int4",    "int8",    "int16"
             ,   "uint",   "uint2",   "uint4",   "uint8",   "uint16"
             ,   "long",   "long2",   "long4",   "long8",   "long16"
             ,  "ulong",  "ulong2",  "ulong4",  "ulong8",  "ulong16"
             ,  "float",  "float2",  "float4",  "float8",  "float16"
             ,   "half",   "half2",   "half4",   "half8",   "half16"
             ,   "quad",   "quad2",   "quad4",   "quad8",   "quad16" ]].

id_test () ->
    ?assertMatch({ok,[{id,_,"arg1"},{id,_,"arg_2"},{id,_,"funfunfun"}],_},
        ice_scan:string(" arg1 arg_2 funfunfun ")).

bool_test_ () ->
    [ ?_assertMatch({ok,[{bool,_,"true"}],_},  ice_scan:string(" true "))
    , ?_assertMatch({ok,[{bool,_,"false"}],_}, ice_scan:string(" false ")) ].

int_test_ () ->
    [ ?_assertMatch({ok,[{int,_,"0"}],_},          ice_scan:string(" 0 "))
    , ?_assertMatch({ok,[{'-',_},{int,_,"10"}],_}, ice_scan:string(" -10 "))
    , ?_assertMatch({ok,[{int,_,"02FEEDface"}],_}, ice_scan:string(" 02FEEDface ")) ].

float_test_ () ->
    [ ?_assertMatch({ok,[{float,_,"0.0f0"}],_},          ice_scan:string(" 0.0f0 "))
    , ?_assertMatch({ok,[{'-',_},{float,_,"10.0f0"}],_}, ice_scan:string(" -10.0f0 "))
    , ?_assertMatch({ok,[{float,_,"42.42f2"}],_},        ice_scan:string(" 42.42f2 ")) ].

char_test_ () ->
    [ ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_}, ice_scan:string(" '' "))
    , ?_assertMatch({ok,[{char,_,"'d'"}],_},              ice_scan:string(" 'd' "))
    , ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_}, ice_scan:string(" 'dd' "))
    , ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_}, ice_scan:string(" ''' "))
    , ?_assertMatch({ok,[{char,_,"'\\''"}],_},            ice_scan:string(" '\\'' ")) ].

raw_string_test_ () ->
    [ ?_assertMatch({ok,[{raw_string,_,"``"}],_},         ice_scan:string(" `` "))
    , ?_assertMatch({ok,[{raw_string,_,"`d`"}],_},        ice_scan:string(" `d` "))
    , ?_assertMatch({ok,[{raw_string,_,"`dd`"}],_},       ice_scan:string(" `dd` "))
    , ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_}, ice_scan:string(" ``` "))
    , ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_}, ice_scan:string(" `\\`` ")) ].

cooked_string_test_ () ->
    [ ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_},        ice_scan:string(" \"\" "))
    , ?_assertMatch({ok,[{cooked_string,_,"\"d\""}],_},          ice_scan:string(" \"d\" "))
    , ?_assertMatch({ok,[{cooked_string,_,"\"dd\""}],_},         ice_scan:string(" \"dd\" "))
    , ?_assertMatch({error,{_,_Lexer,{illegal,_Part}},_},        ice_scan:string(" \"\"\" "))
    , ?_assertMatch({ok,[{cooked_string,_,"\"\\\"\""}],_},       ice_scan:string(" \"\\\"\" ")) %% = ‘ "\"" ’
    , ?_assertMatch({ok,[{cooked_string,_,"\"ddd\\\"ddd\""}],_}, ice_scan:string(" \"ddd\\\"ddd\" ")) ]. %% = ‘ "ddd\"ddd" ’

%% Internals

%% End of Module.
