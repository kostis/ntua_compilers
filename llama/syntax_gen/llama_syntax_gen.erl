%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% 
%%% @doc A random generator of Abstract Syntax Trees of syntactically
%%%      valid Llama programs.
%%%
%%% Generate the AST of a random program by:
%%%    proper_gen:pick(llama_syntax_gen:program()).
%%% and repeat issuing this call as many times as you like.
%%%
%%% Alternatively, pretty-print a random Llama program on stdout by:
%%%    llama_pp:p().
%%% and repeat this until you are satisfied (or bored).
%%% 
%%% -------------------------------------------------------------------
-module(llama_syntax_gen).

-export([program/0]).

-include_lib("proper/include/proper.hrl").

prop_gen() ->
  ?FORALL(P, program(), erlang:display(P)).

%%
%% Some convenient utilities
%%
-define(STAR(MAX, GEN), ?LET(N, integer(0, MAX), vector(N, GEN))).
-define(PLUS(MAX, GEN), ?LET(N, integer(1, MAX), vector(N, GEN))).

%%
%% Arbitrary limits
%%
-define(MAX_ARITY,   3).  % for Patterns
-define(MAX_ARR_DIM, 2).
-define(MAX_CLAUSES, 3).
-define(MAX_CONSTRS, 4).
-define(MAX_DEFS,    5).
-define(MAX_PARAMS,  4).

program() ->
  M = 3 + rand:uniform(7),  % M ranges in 4..10
  frequency([{ 1, vector(0, whatever)}, % the empty program is legal
	     {14, vector(1, letdef())},
	     { 8, vector(2, letdef())},
	     {18, vector(3, union([letdef(), typedef()]))},
	     { 2, vector(M, union([letdef(), typedef()]))}]).

letdef() ->
  ?SIZED(Size, letdef(Size)).

letdef(Size) ->
  frequency([{5, {'let', defs(Size)}},
	     {1, {'letrec', defs(Size)}}]).

defs(Size) ->
  ?PLUS(?MAX_DEFS, def(Size div 2)).

def(Size) ->
  frequency([{9, {'def', id(), pars(), opt_type(), ?LAZY(expr(Size))}},
	     {1, {'mutable', id(), exprs(Size, 0, ?MAX_ARR_DIM), opt_type()}}]).

typedef() ->
  {'type', frequency([{7, vector(1, tdef())},
		      {2, vector(2, tdef())},
		      {1, vector(3, tdef())}])}.

tdef() ->
  {'tdef', id(), ?PLUS(?MAX_CONSTRS, constr())}.

constr() ->
  frequency([{7, {'constr', 'Id'()}},
	     {3, {'constr', 'Id'(), ?PLUS(?MAX_PARAMS, type())}}]).

pars() ->
  ?STAR(?MAX_PARAMS, par()).

par() ->
  frequency([{4, id()},
	     {1, {'paren', id(), type()}}]).

opt_type() ->
  frequency([{4, ''},
	     {1, type()}]).

type() ->
  union(['unit', 'int', 'char', 'bool', 'float',
	 {'paren', ?LAZY(type())},
	 {'arrow', ?LAZY(type()), ?LAZY(type())},
	 {'ref', ?LAZY(type())},
	 {'array', union([unknown, integer(1, ?MAX_ARR_DIM)]), ?LAZY(type())},
	 id()]).

%% expr() ->
%%   ?SIZED(Size, expr(Size)).

expr(Size) when Size =< 1 ->
  union([int_const(), float_const(), char_const(), string_literal(),
	 boolean(), '()']);
expr(Sz) ->
  Sz1 = Sz - 1, Sz2 = Sz div 2, Sz4 = Sz div 4,
  union([{'paren', ?LAZY(expr(Sz1))},
	 {'unop', unop(), ?LAZY(expr(Sz1))},
	 {'binop', binop(), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))},
	 {'app', union([id(), 'Id'()]), exprs(Sz1, 0, ?MAX_ARITY)},
	 {'arr', id(), exprs(Sz1, 1, ?MAX_ARR_DIM)},
	 {'dim', union([unknown, integer(1, ?MAX_ARR_DIM)]), id()},
	 {'new', type()},
	 {'delete', ?LAZY(expr(Sz1))},
	 {'letdef', ?LAZY(letdef(Sz2)), ?LAZY(expr(Sz2))},
	 {'begin_end', ?LAZY(expr(Sz1))},
	 {'if', ?LAZY(expr(Sz4)), ?LAZY(expr(Sz2)), union(['()', ?LAZY(expr(Sz2))])},
	 {'while', ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))},
	 {'for', id(), ?LAZY(expr(Sz4)), dir(), ?LAZY(expr(Sz4)), ?LAZY(expr(Sz2))},
	 {'match', ?LAZY(expr(Sz2)), clauses(Sz2)}]).

exprs(Sz, Min, Max) ->
  ?LET(N, integer(Min, Max), vector(N, expr(Sz))).

dir() ->
  union(['to', 'downto']).

unop() ->
  union(['+', '-', '+.', '-.', '!', 'not']).

binop() ->
  union(['+', '-', '*', '/', '+.', '-.', '*.', '/.', 'mod', '++', '=', '<>',
	 '<', '>', '<=', '>=', '==', '!=', '&&', '||', ';', ':=']).

clause(Size) ->
  {clause, pattern(), expr(Size)}.

clauses(Size) ->
  ?PLUS(?MAX_CLAUSES, clause(Size)).

pattern() ->
  union([int_patt(), float_patt(), char_const(),
	 boolean(), id(), {'paren', ?LAZY(pattern())}, {'Id'(), patt_args()}]).

patt_args() ->
  ?STAR(?MAX_ARITY, pattern()).

int_patt() ->
  union([int_const(), {'+', int_const()}, {'-', int_const()}]).

float_patt() ->
  union([float_const(), {'+.', float_const()}, {'-.', float_const()}]).

%% Lexical terminals below

id() ->
  union(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'foo', 'bar', 'main']).

'Id'() ->
  union(['A', 'B', 'C', 'D', 'Nil', 'Cons', 'Empty', 'Tree']).

int_const() ->
  integer(0, 42).

float_const() ->
  union([0.0, 2.56, 3.14, 0.420e+2, 42000.0e-3]).

char_const() ->  % These are special-cased in llama_pp!
  {'char', union(['a', '7', '\n', '\''])}.

string_literal() ->
  {'string', union(["foo", "bar", "Route66",
		    "Name:\t\"DouglasAdams\"\nValue:\t42\n"])}.
