%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% 
%%% @doc A random generator of Abstract Syntax Trees of syntactically
%%%      valid Mini-Basic programs.
%%%
%%% Generate the AST of a random program by:
%%%    proper_gen:pick(minibasic_syntax_gen:program()).
%%% and repeat issuing this call as many times as you like.
%%%
%%% Alternatively, pretty-print a random Mini-Basic program on stdout by:
%%%    minibasic_pp:p().
%%% and repeat this until you are satisfied (or bored).
%%% 
%%% -------------------------------------------------------------------
-module(minibasic_syntax_gen).

-export([program/0]).

-include_lib("proper/include/proper.hrl").

prop_gen() ->
  ?FORALL(P, program(), erlang:display(P)).

%%
%% Some convenient utilities
%%
-define(OPT(GEN), union(['', GEN])).
-define(STAR(MAX, GEN), ?LET(N, integer(0, MAX), vector(N, GEN))).

program() ->
  ?SIZED(Sz, stmt_list(Sz)).

stmt_list(Sz) ->
  Sz2 = Sz div 2,
  ?STAR(Sz, stmt(Sz2)).

stmt(Sz) ->
  Sz2 = Sz div 2,
  union([{'let', id(), expr(Sz)},
	 {'if',  expr(Sz), ?LAZY(stmt(Sz2)), ?OPT(?LAZY(stmt(Sz2)))},
	 {'for', expr(Sz), ?LAZY(stmt(Sz2))},
	 {'blk', ?LAZY(stmt_list(Sz2))}]).

expr(Size) when Size =< 1 ->
  union([id(), int_const()]);
expr(Sz) ->
  Sz2 = Sz div 2,
  union([{'prn', ?LAZY(expr(Sz2))},
	 {'bop', bin_op(), ?LAZY(expr(Sz2)), ?LAZY(expr(Sz2))}]).

bin_op() ->
  union(['+', '-', '*', '/', '%']).

%% Lexical terminals below -- they can be extended at will

id() ->
  union(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).

int_const() ->
  integer(0, 42).
