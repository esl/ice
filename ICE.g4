grammar ICE;

/// Root rule
root : (block separator?)+ ;

block : declaration | expr ;

separator : ';;' | '%%' ;

/// Tokens

Comment : ('//' (~[\r\n])* '\r'? '\n')+ -> skip ;

BlankOrUnprintable : [\u0000-\u0020] -> skip ;

id : Id ;
Id : [a-zA-Z_] [a-zA-Z0-9_]* ;

bool : Bool ;
Bool : 'true' | 'false' ;

tokInt : TokInt ;
TokInt : ([0-9]+ | '0'[2-9A-Za-z]+) ;

tokFloat : TokFloat ;
TokFloat : [0-9]+ '.' [0-9]+ 'f' [+-]? [0-9]+ ;

tokChar : TokChar ;
TokChar : '\'' ( '\\' (~'\\'|'\\') | ~[\\''] ) '\'' ;

rawString : RawString ;
RawString : '`' (~'`')* '`' ;

cookedString : CookedString ;
CookedString : '"' ( '\\' (~'\\'|'\\') | ~[\\"] )+ '"' ;

leftwardsArrow : LeftwardsArrow ;
LeftwardsArrow : '<-' | [\u2190] ; // \u2190 = '←'
rightwardsArrow : RightwardsArrow ;
RightwardsArrow : '->' | [\u2192] ; // \u2192 = '→'

crIntensionKwd : UpwardsArrow | 'i^' ;
UpwardsArrow : [\u2191] ;   // \u2191 = ‘↑’
evIntensionKwd : DownwardsArrow | 'i!' ;
DownwardsArrow : [\u2193] ; // \u2193 = ‘↓’


/// Declarations

declaration : dim_decl | var_decl | fun_decl | ext_decl ;

dim_decl : 'dim'        id                                              (leftwardsArrow expr)? ;
var_decl : 'var'        id                                                          '=' expr ;
fun_decl : 'fun'        id        args   ('[' tguards ']')?   ('|' pguard)?         '=' expr ;
ext_decl : 'ext' tokInt id homogeneous_to '(' tyAssoc+ ')' rightwardsArrow clScalar '=' expr ;

args : (     id     // named_param
       | '.' id     // base_param
       | '!' id)+ ; // value_param
 // Note how functions have arity ≥ 1.
params : (     exprMax     // named_param
         | '.' exprMax     // base_param
         | '!' exprMax)+ ; // value_param

tguards : tguard (',' tguards)* ;
tguard : id ':' range_operator ;
pguard : expr100 ;

homogeneous_to : '::' ;
tyAssoc : id homogeneous_to clScalar ;
clScalar :   'bool' |   'bool2' |   'bool4' |   'bool8' |   'bool16'
         |   'char' |   'char2' |   'char4' |   'char8' |   'char16'
         |  'uchar' |  'uchar2' |  'uchar4' |  'uchar8' |  'uchar16'
         | 'double' | 'double2' | 'double4' | 'double8' | 'double16'
         |  'short' |  'short2' |  'short4' |  'short8' |  'short16'
         | 'ushort' | 'ushort2' | 'ushort4' | 'ushort8' | 'ushort16'
         |    'int' |    'int2' |    'int4' |    'int8' |    'int16'
         |   'uint' |   'uint2' |   'uint4' |   'uint8' |   'uint16'
         |   'long' |   'long2' |   'long4' |   'long8' |   'long16'
         |  'ulong' |  'ulong2' |  'ulong4' |  'ulong8' |  'ulong16'
         |  'float' |  'float2' |  'float4' |  'float8' |  'float16'
         |   'half' |   'half2' |   'half4' |   'half8' |   'half16'
         |   'quad' |   'quad2' |   'quad4' |   'quad8' |   'quad16' ;

where_end : 'where' (declaration separator?)* 'end' ;

/// Expressions

expr : expr where_end
     | expr10
     ;

// Kept to better display rule order
expr10 : bind_operator ;
expr100 : or_operator ;
expr150 : and_operator ;
expr200 : comparison_operators ;
//expr400 : low_priority_operators ;
//expr500 : high_priority_operators ;
expr550 : context_perturbation ;
expr552 : range_operator ;
expr555 : unary_operators ;
expr650 : function_call ;
expr700 : exprMax ;

exprMax : id
        | atomic
        | tuple          // Tuple builder
        | crIntension    // Intension creation
        | evIntension    // Intension evaluation
        | braced_expr    // expr inside parenthesis
        | conditional    // If-Then(-Elsif-Then)?-Else-Fi
        | lambda         // Lambda expressions
        | context_query  // #.smth and #!smth
        ;
exprMaxs : exprMax (',' exprMax)* ;

tuple : '[' ltrAssoc (',' ltrAssoc)* ']' ;
ltrAssoc : expr leftwardsArrow expr ;

crIntension :
    crIntensionKwd '{'  exprMaxs? '}' expr ;

evIntension :
    evIntensionKwd      exprMax ;

braced_expr : '(' expr ')' ;

conditional : 'if' expr 'then' expr elsif* 'else' expr 'fi' ;
elsif :    'elsif' expr 'then' expr ;

lambda : '\\' frozenDims? params rightwardsArrow expr ;
frozenDims : '{' exprMaxs '}' ;

context_query : contextOp (id | braced_expr) ;

bind_operator :              expr100     ';' expr10
                           | expr100 ;
or_operator :                expr150    orOp expr100
                           | expr150 ;
and_operator :               expr200   andOp expr150
                           | expr200 ;
comparison_operators :       expr400   cmpOp expr400
                           | expr400 ;
expr400 :                    expr400   addOp expr500
                           |                 expr500 ;
expr500 :                    expr500   mulOp expr550
                           |                 expr550 ;
context_perturbation :       expr552     '@' expr550
                           | expr552 ;
range_operator :             expr555 rangeOp expr550
                           | expr555 ;
unary_operators :  unaryOp   expr650
                           | expr650 ;
function_call :              expr700 params
                           | expr700 ;

/// Basic data

atomic : bool
       | tokInt
       | tokFloat
       | tokChar
       | rawString
       | cookedString
       ;


/// Operators

contextOp : '#.' | '#!' ;

orOp : '||' | 'or' ;

andOp : '&&' | 'and' ;

cmpOp : '<' | '<=' | '==' | '>=' | '>' | '!=' ;

addOp : '+' | '-' ;

mulOp : '*' | '/' | '%' ;

rangeOp : '..' ;

unaryOp : '-' | '+' | 'not' ;

