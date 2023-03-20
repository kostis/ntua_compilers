%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% 
%%% @doc A random generator of Abstract Syntax Trees of syntactically
%%%      valid Grace programs.
%%%
%%% Generate the AST of a random program by:
%%%    proper_gen:pick(grace_syntax_gen:program()).
%%% and repeat issuing this call as many times as you like.
%%%
%%% Alternatively, pretty-print a random Grace program on stdout by:
%%%    grace_pp:p().
%%% and repeat this until you are satisfied (or bored).
%%% 
%%% -------------------------------------------------------------------
-module(grace_syntax_gen).

-export([program/0]).

-include_lib("proper/include/proper.hrl").

prop_gen() ->
  ?FORALL(P, program(), erlang:display(P)).

%%
%% Some convenient utilities
%%
-define(OPT(GEN), union(['', GEN])).
-define(STAR(MAX, GEN), ?LET(N, integer(0, MAX), vector(N, GEN))).
-define(PLUS(MAX, GEN), ?LET(N, integer(1, MAX), vector(N, GEN))).

%%
%% Arbitrary limits
%%
-define(MAX_DECLS,   5).
-define(MAX_PARAMS,  4).
-define(MAX_STMTS,   5).

program() ->
  M = 3 + rand:uniform(8),  % M ranges in 4..11
  frequency([{1, vector(1, decl())},
	     {2, vector(2, decl())},
	     {3, vector(3, decl())},
	     {4, vector(M, decl())}]).

decl() ->
  frequency([{1, var_decl()}, {1, fun_decl()}, {3, fun_def()}]).

var_decl() ->
  {'vardecl', type(), declarators()}.

type() ->
  {'type', basic_type(), stars()}.

basic_type() ->
  union(['int', 'char', 'bool', 'double']).

stars() ->
  frequency([{6,''}, {4,'*'}, {2,'**'}, {1,'***'}]).

declarators() ->
  ?PLUS(?MAX_DECLS, declarator()).

declarator() ->
  frequency([{9, {'decl','I'(),''}}, {1, {'decl','I'(),const_expr()}}]).

fun_decl() ->
  {'fundecl', res_type(), 'I'(), param_list()}.

res_type() ->
  frequency([{5, type()}, {1, {'type','void',''}}]).

param_list() ->
  ?PLUS(?MAX_PARAMS, param()).

param() ->
  {'param', ?OPT('byref '), type(), 'I'()}.

fun_def() ->
  {'fundef', res_type(), 'I'(), param_list(),
   ?STAR(?MAX_DECLS div 2, decl()), ?STAR(?MAX_STMTS, stmt())}.

stmt() ->
  ?SIZED(Size, stmt(Size)).

stmt(Sz) ->
  Sz2 = Sz div 2,
  S = union(['', expr(Sz),
	     {'blck', ?STAR(?MAX_STMTS div 2, ?LAZY(stmt(Sz2)))},
	     {'if', expr(1), ?LAZY(stmt(Sz2)), ?OPT(?LAZY(stmt(Sz2)))},
	     {'for', ?OPT('I'()), ?OPT(expr(1)), ?OPT(expr(1)), ?OPT(expr(1)),
	      ?LAZY(stmt(Sz2))},
	     {'cnt', ?OPT('I'())}, {'brk', ?OPT('I'())},
	     {'ret', ?OPT(expr(1))}]),
  {'stmt', S}.

expr(Size) when Size =< 1 ->
  union(['I'(), 'true', 'false', 'NULL',
	 int_const(), char_const(), double_const(), string_literal()]);
expr(Sz) ->
  Sz1 = Sz - 1, Sz2 = Sz div 2, Sz4 = Sz div 4,
  union([{'prn', ?LAZY(expr(Sz1))},
	 {'cll', 'I'(), exprs(Sz1, 1, ?MAX_PARAMS)},
	 {'arr', ?LAZY(expr(Sz4)), ?LAZY(expr(Sz4))},
	 {'uop', un_op(), ?LAZY(expr(Sz1))},
	 {'bop', bin_op(), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))},
	 {'uas', un_asgn(), ?LAZY(expr(Sz4)), oneof(['pre', 'post'])},
	 {'bas', bin_asgn(), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))},
	 {'cst', type(), ?LAZY(expr(Sz1))},
	 {'cnd', ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))},
	 {'new', type(), ?OPT(?LAZY(expr(Sz2)))},
 	 {'del', ?LAZY(expr(Sz2))}]).

exprs(Sz, Min, Max) ->
  ?LET(N, integer(Min, Max), vector(N, expr(Sz))).

const_expr() ->
  expr(1).

un_op() ->
  union(['&', '*', '+', '-', '!']).

bin_op() ->
  union(['*', '/', '%', '+', '-', '<', '>', '<=', '>=', '==', '!=',
	 '&&', '||', ',']).

un_asgn() ->
  union(['++', '--']).

bin_asgn() ->
  union(['=', '*=', '/=', '%=', '+=', '-=']).

%% Lexical terminals below -- they can be extended at will

'I'() ->
  union(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
	 'x', 'y', 'z', 'foo', 'bar', 'baz', 'boo', 'main']).

int_const() ->
  integer(0, 42).

double_const() ->
  union([0.0, 2.56, 3.14, 0.420e+2, 42000.0e-3]).

char_const() ->  % These are special-cased in grace_pp!
  {'char', union(['a', '7', '\n', '\''])}.

string_literal() ->
  {'string', union(["foo", "bar", "Route66",
		    "Name:\t\"DouglasAdams\"\nValue:\t42\n"])}.
