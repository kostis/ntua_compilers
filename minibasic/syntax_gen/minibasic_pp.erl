%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%%
%%% @doc A (not so pretty) pretty-printer of Mini-Basic programs.
%%%
%%% <p>It exports three functions:
%%%   1. pp/1, which returns an IO list of the string of a Mini-Basic program
%%%   2. p/0, which prints a Mini-Basic program on stdout
%%%   3. gen/1, which generates N programs under a `programs' directory
%%% The first of these functions is general; the others have a hard-coded
%%% call to the generator of syntactically correct Mini-Basic programs.  This
%%% needs to be made more general, at some point.</p>
%%%
%%% TODO: Some expressions may still need to be enclosed in parentheses.
%%%
%%% -------------------------------------------------------------------
-module(minibasic_pp).

-export([gen/1, p/0, pp/1]).

-define(F(S, L), io_lib:format(S, L)).

-spec gen(non_neg_integer()) -> 'ok'.

gen(0) -> ok;
gen(N) ->
  FN = "programs/p"++integer_to_list(N)++".mba",
  {ok, P} = proper_gen:pick(minibasic_syntax_gen:program()),
  ok = file:write_file(FN, pp(P)),
  gen(N - 1).

-spec p() -> 'ok'.

p() ->
  {ok, P} = proper_gen:pick(minibasic_syntax_gen:program()),
  io:format(pp(P)).

-spec pp(_) -> iolist().  % XXX: define a type for Mini-Basic abstract syntax

pp([]) -> "";
pp([H|T]) -> ?F("~s~n~s", [pp(H), pp(T)]);
%% Statements
pp({'let', I, Expr}) -> ?F("let ~s = ~s", [pp(I), pp(Expr)]);
pp({'if',  Expr, Stmt1, ''}) -> ?F("if ~s then ~s", [pp(Expr), pp(Stmt1)]);
pp({'if',  Expr, Stmt1, Stmt2}) ->
  ?F("if ~s then ~s else ~s", [pp(Expr), pp(Stmt1), pp(Stmt2)]);
pp({'for', Expr, Stmt}) -> ?F("for ~s do ~s", [pp(Expr), pp(Stmt)]);
pp({'blk', Stmts}) -> ?F("begin ~s end", [pp(Stmts)]);
%% Expressions
pp({'prn', E}) -> ?F("(~s)", [pp(E)]);
pp({'bop', Op, E1, E2}) -> ?F("(~s ~s ~s)", [pp_pe(E1), pp(Op), pp_pe(E2)]);
pp(A) when is_atom(A) -> ?F("~s", [A]);
pp(N) when is_integer(N) -> ?F("~p", [N]).

%%
%% Exists only for determining when to put parentheses around an expression.
%% Simple expressions (identifiers and constants) are not parenthesized.
%%
pp_pe(A) when is_atom(A) -> pp(A);
pp_pe(N) when is_integer(N) -> pp(N);
pp_pe(Expr) -> ?F("(~s)", [pp(Expr)]).
