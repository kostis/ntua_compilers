%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%%
%%% @doc A (not so pretty) pretty-printer of Grace programs.
%%%
%%% <p>It exports three functions:
%%%   1. pp/1, which returns an IO list of the string of a Grace program
%%%   2. p/0, which prints a Grace program on stdout
%%%   3. gen/1, which generates N programs under a `programs' directory
%%% The first of these functions is general; the others have a hard-coded
%%% call to the generator of syntactically correct Grace programs.  This
%%% needs to be made more general, at some point.</p>
%%%
%%% TODO: Some expressions may still need to be enclosed in parentheses.
%%%
%%% -------------------------------------------------------------------
-module(grace_pp).

-export([gen/1, p/0, pp/1]).

-define(F(S, L), io_lib:format(S, L)).

-spec gen(non_neg_integer()) -> 'ok'.

gen(0) -> ok;
gen(N) ->
  FN = "programs/p"++integer_to_list(N)++".eds",
  {ok, P} = proper_gen:pick(edsger_syntax_gen:program()),
  ok = file:write_file(FN, pp(P)),
  gen(N - 1).

-spec p() -> 'ok'.

p() ->
  {ok, P} = proper_gen:pick(edsger_syntax_gen:program()),
  io:format(pp(P)).

-spec pp(_) -> iolist().  % XXX: define a type for Grace abstract syntax

pp([]) -> "";
pp([H|T]) -> ?F("~s~n~s", [pp(H), pp(T)]);
pp({'vardecl', Type, Decls}) -> ?F("~s ~s;", [pp(Type), pp_decls(Decls)]);
pp({'fundecl', RT, I,  Params}) ->
  ?F("~s ~s(~s);", [pp(RT), I, pp_params(Params)]);
pp({'fundef', RT, I,  Params, Decls, Stmts}) ->
  ?F("~s ~s(~s) {~n~s~n~s}",
     [pp(RT), I, pp_params(Params), pp(Decls), pp(Stmts)]);
pp({'type', Type, Stars}) -> ?F("~s~s", [Type, Stars]);
pp({'stmt', {'blck', Stmts}}) -> ?F("{~n ~s~n}", [pp(Stmts)]);
pp({'stmt', {'if', Expr, Stmt1, Stmt2}}) ->
  ?F("if (~s) ~s~s", [pp(Expr), pp(Stmt1), pp_opt_else(Stmt2)]);
pp({'stmt', {'for', I, Expr1, Expr2, Expr3, Stmt}}) ->
  ?F("~sfor (~s; ~s; ~s) ~s",
     [pp_opt_lbl(I), pp(Expr1), pp(Expr2), pp(Expr3), pp(Stmt)]);
pp({'stmt', Stmt}) -> ?F("~s;", [pp(Stmt)]);  % all other statements need ;
pp({'cnt', I}) -> ?F("continue~s", [pp_opt(I)]);
pp({'brk', I}) -> ?F("break~s", [pp_opt(I)]);
pp({'ret', Expr}) -> ?F("return~s", [pp_opt(Expr)]);
pp({'prn', E}) -> ?F("(~s)", [pp(E)]);
pp({'cll', I, Es}) -> ?F("~s(~s)", [pp(I), pp_exprs(Es)]);
pp({'arr', E1, E2}) -> ?F("~s[~s]", [pp(E1), pp(E2)]);
pp({'uop', Op, E}) -> ?F("~s ~s", [pp(Op), pp_pe(E)]);
pp({'bop', Op, E1, E2}) -> ?F("(~s ~s ~s)", [pp_pe(E1), pp(Op), pp_pe(E2)]);
pp({'uas', As, E, 'pre'}) -> ?F("~s~s", [pp(As), pp_pe(E)]);
pp({'uas', As, E, 'post'}) -> ?F("~s~s", [pp_pe(E), pp(As)]);
pp({'bas', As, E1, E2}) -> ?F("(~s ~s ~s)", [pp_pe(E1), pp(As), pp_pe(E2)]);
pp({'cst', Type, E}) -> ?F("(~s)~s", [pp(Type), pp_pe(E)]);
pp({'cnd', E1, E2, E3}) ->
  ?F("~s ? ~s : ~s", [pp_pe(E1), pp_pe(E2), pp_pe(E3)]);
pp({'new', T, Expr}) -> ?F("new ~s~s", [pp(T), pp_opt_arrexpr(Expr)]);
pp({'del', Expr}) -> ?F("delete ~s", [pp(Expr)]);
pp({'string', S}) -> ?F("~p", [S]);
pp({'char', C}) -> pp_char(C);
pp(N) when is_number(N) -> ?F("~p", [N]);
pp(A) when is_atom(A) -> ?F("~s", [A]).

pp_exprs([E]) -> ?F("~s", [pp(E)]);
pp_exprs([E|Es]) -> ?F("~s, ~s", [pp(E), pp_exprs(Es)]).

pp_decls([D]) -> ?F("~s", [pp_decl(D)]);
pp_decls([D|Ds]) -> ?F("~s, ~s", [pp_decl(D), pp_decls(Ds)]).

pp_decl({'decl', I, Expr}) -> ?F("~s~s", [I, pp_opt_arrexpr(Expr)]).

pp_opt('') -> "";
pp_opt(X) -> ?F(" ~s", [pp(X)]).

pp_opt_else('') -> "";
pp_opt_else(Stmt) -> ?F(" else ~s", [pp(Stmt)]).

pp_opt_lbl('') -> "";
pp_opt_lbl(I) -> ?F("~s: ", [pp(I)]).

pp_opt_arrexpr('') -> "";
pp_opt_arrexpr(Expr) -> ?F("[~s]", [pp(Expr)]).

%% pp_params([]) -> "";
pp_params([P]) -> ?F("~s", [pp_param(P)]);
pp_params([P1|Ps]) -> ?F("~s, ~s", [pp_param(P1), pp_params(Ps)]).

pp_param({'param', P, Type, I}) -> ?F("~s~s ~s", [P, pp(Type), I]).


%%
%% Exists only for determining when to put parentheses around an expression.
%% Simple expressions are not parenthesized.
%%
pp_pe({'string', _}=E) -> pp(E);
pp_pe({'char', _}=E) -> pp(E);
pp_pe(N) when is_number(N) -> pp(N);
pp_pe(A) when is_atom(A) -> pp(A);
pp_pe(E) -> ?F("(~s)", [pp(E)]).

%%
%% The pretty-printing of chars is hard-coded
%% (due to Erlang printing chars differently),
%%
pp_char('a') -> "'a'";
pp_char('7') -> "'7'";
pp_char('\n') -> "'\\n'";
pp_char('\'') -> "'\\''".
