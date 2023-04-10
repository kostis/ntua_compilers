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
-define(MAX_PARAMS,  3).
-define(MAX_STMTS,   5).

program() ->
  func_def().

func_def() ->
  {'fundef', header(), ?STAR(?MAX_PARAMS, local_def()), block()}.

header() ->
  {'fun', 'I'(), ?STAR(?MAX_DECLS, fpar_def()), ret_type()}.

fpar_def() ->
  {'fpar_def', ?OPT('ref'), ?PLUS(?MAX_PARAMS, 'I'()), fpar_type()}.

type() ->
  {'type', data_type(), brackets()}.

data_type() ->
  union(['int', 'char']).

brackets() ->
  frequency([{6, ''}, {3, int_const()}, {1, {int_const(), int_const()}}]).

ret_type() ->
  union([data_type(), 'nothing']).

fpar_type() ->
  {'fpar_type', data_type(), ?OPT('[]'), brackets()}.

local_def() ->
  frequency([{3, func_def()}, {1, func_decl()}, {1, var_def()}]).

func_decl() ->
  {'fundecl', header()}.

var_def() ->
  {'var', ?PLUS(3, 'I'()), type()}.

%% stmt() ->
%%   ?SIZED(Size, stmt(Size)).

stmt(Sz) ->
  Sz2 = Sz div 2, Sz4 = Sz div 4,
  union([';',
	 {'<-', ?LAZY(lvalue(Sz4)), ?LAZY(expr(Sz2))},
	 block(),
	 {'stmt', func_call(Sz)},  % 'stmt' tag disambiguates stmts from exprs
	 {'if', ?LAZY(cnd(Sz4)), ?LAZY(stmt(Sz2)), ?OPT(?LAZY(stmt(Sz2)))},
	 {'while', ?LAZY(cnd(Sz4)), ?LAZY(stmt(Sz2))},
	 {'ret', ?OPT(expr(Sz2))}]).

block() ->
  ?SIZED(Size, block(Size)).

block(Sz) ->
  Sz2 = Sz div 2,
  {'blck', ?STAR(?MAX_STMTS, ?LAZY(stmt(Sz2)))}.

func_call(Sz) ->
  {'cll', 'I'(), ?STAR(?MAX_PARAMS, ?LAZY(expr(Sz)))}.  % XXX: 1

lvalue(Sz) ->
  Sz2 = Sz div 2,
  frequency([{5, 'I'()},
	     {1, string_literal()},
	     {1, {lval, ?LAZY(lvalue(Sz2)), ?LAZY(expr(Sz2))}}]). 

expr(Size) when Size =< 1 ->
  union([int_const(), char_const()]);
expr(Sz) ->
  Sz1 = Sz - 1, Sz2 = Sz div 2,
  union([{'prn', ?LAZY(expr(Sz2))},
	 ?LAZY(lvalue(Sz)),
	 func_call(Sz),
	 {'uop', un_op(), ?LAZY(expr(Sz1))},
	 {'bop', bin_op(), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))}]).

cnd(Sz) when Sz =< 1 -> % Base case without self-recursion
  {'cmp', cmp_op(), expr(Sz), expr(Sz)};
cnd(Sz) ->
  Sz2 = Sz div 2,
  union([{'prn', ?LAZY(cnd(Sz2))},
	 {'not', ?LAZY(cnd(Sz2))},
	 {'and', ?LAZY(cnd(Sz2)), ?LAZY(cnd(Sz2))},
	 {'or',  ?LAZY(cnd(Sz2)), ?LAZY(cnd(Sz2))},
	 {'cmp', cmp_op(), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))}]).

un_op() ->
  union(['+', '-']).

bin_op() ->
  union(['+', '-',  '*', 'div', 'mod']).

cmp_op() ->
  union(['=', '#', '<', '>', '<=', '>=']).

%% Lexical terminals below -- they can be extended at will

'I'() ->
  union(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
	 'x', 'y', 'z', 'foo', 'bar', 'baz', 'boo', 'main']).

int_const() ->
  integer(0, 42).

char_const() ->  % These are special-cased in grace_pp!
  {'char', union(['a', '7', '\n', '\''])}.

string_literal() ->
  {'string', union(["foo", "bar", "Route66",
		    "Name:\t\"DouglasAdams\"\nValue:\t42\n"])}.
