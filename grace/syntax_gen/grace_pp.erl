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
  FN = "programs/p"++integer_to_list(N)++".grc",
  {ok, P} = proper_gen:pick(grace_syntax_gen:program()),
  ok = file:write_file(FN, pp(P)),
  gen(N - 1).

-spec p() -> 'ok'.

p() ->
  {ok, P} = proper_gen:pick(grace_syntax_gen:program()),
  io:format(pp(P)).

-spec pp(_) -> iolist().  % XXX: define a type for Grace abstract syntax

pp([]) -> "";
pp([H|T]) -> ?F("~s~n~s", [pp(H), pp(T)]);
%% fun stuff
pp({'fun', I,  FParDefs, RT}) ->
  ?F("fun ~s(~s) : ~s", [I, pp_fpars(FParDefs), pp(RT)]);
pp({'fundef', Header,  LocalDefs, Block}) ->
  ?F("~s~n~s~n~s~n", [pp(Header), pp(LocalDefs), pp(Block)]);
pp({'fundecl', Header}) -> ?F("~s;", [pp(Header)]);
pp({'type', Type, Brackets}) -> ?F("~s~s", [Type, pp_brackets(Brackets)]);
pp({'fpar_type', Type, Opt, Brackets}) ->
  ?F("~s~s~s", [Type, Opt, pp_brackets(Brackets)]);
%% statements
pp(';') -> ?F(";", []);  % although it is captured by atom rule below
pp({'<-', LVal, Expr}) -> ?F("~s <- ~s;", [pp(LVal), pp(Expr)]);
pp({'blck', Stmts}) -> ?F("{~n~s~n}", [pp_block(Stmts)]);
pp({'stmt', FuncCall}) -> ?F("~s;", [pp(FuncCall)]);
pp({'if', Cond, Stmt1, Stmt2}) ->
  ?F("if ~s then ~s~s", [pp(Cond), pp(Stmt1), pp_opt_else(Stmt2)]);
pp({'while', Cond, Stmt}) -> ?F("while ~s do ~s", [pp(Cond), pp(Stmt)]);
pp({'ret', Expr}) -> ?F("return~s;", [pp_opt(Expr)]);
%% conds
pp({'not', Cond}) -> ?F("not ~s", [pp(Cond)]);
pp({'and', Cond1, Cond2}) -> ?F("~s and ~s", [pp(Cond1), pp(Cond2)]);
pp({'or',  Cond1, Cond2}) -> ?F("~s or ~s", [pp(Cond1), pp(Cond2)]);
pp({'cmp', CompOp, E1, E2}) -> ?F("~s ~s ~s", [pp(E1), pp(CompOp), pp(E2)]);
%% exprs
pp({'uop', UnOp, E}) -> ?F("~s ~s", [pp(UnOp), pp_pe(E)]);
pp({'bop', BOp, E1, E2}) -> ?F("(~s ~s ~s)", [pp_pe(E1), pp(BOp), pp_pe(E2)]);
%% misc
pp({'prn', E}) -> ?F("(~s)", [pp(E)]);
pp({'lval', LVal, Expr}) -> ?F("~s[~s]", [pp(LVal), pp(Expr)]);
pp({'cll', ID, Es}) -> ?F("~s(~s)", [pp(ID), pp_exprs(Es)]);
%% basic elements
pp({'var', IDs, Type}) -> ?F("var ~s : ~s;", [pp_ids(IDs), pp(Type)]);
pp({'string', S}) -> ?F("~p", [S]);
pp({'char', C}) -> pp_char(C);
pp(I) when is_integer(I) -> ?F("~p", [I]);
pp(A) when is_atom(A) -> ?F("~s", [A]).

pp_block([]) -> "";
pp_block([S]) -> ?F("  ~s", [pp(S)]);
pp_block([S|Ss]) -> ?F("~s~n~s", [pp_block([S]), pp_block(Ss)]).

pp_brackets('') -> "";
pp_brackets(I) when is_integer(I) -> ?F("[~p]", [I]);
pp_brackets({I1, I2}) -> ?F("[~p][~p]", [I1, I2]).

pp_ids([ID]) -> ?F("~s", [pp(ID)]);
pp_ids([ID|IDs]) -> ?F("~s, ~s", [pp(ID), pp_ids(IDs)]).

pp_exprs([]) -> "";
pp_exprs([E]) -> ?F("~s", [pp(E)]);
pp_exprs([E|Es]) -> ?F("~s, ~s", [pp(E), pp_exprs(Es)]).

pp_opt('') -> "";
pp_opt(X) -> ?F(" ~s", [pp(X)]).

pp_opt_else('') -> "";
pp_opt_else(Stmt) -> ?F(" else ~s", [pp(Stmt)]).

pp_opt_ref('') -> "";
pp_opt_ref(ref) -> ?F("ref ", []).

pp_fpars([]) -> "";
pp_fpars([FP]) -> ?F("~s", [pp_fpar(FP)]);
pp_fpars([FP1|FPs]) -> ?F("~s; ~s", [pp_fpar(FP1), pp_fpars(FPs)]).

pp_fpar({'fpar_def', Ref, IDs, Type}) ->
  ?F("~s~s : ~s", [pp_opt_ref(Ref), pp_ids(IDs), pp(Type)]).


%%
%% Exists only for determining when to put parentheses around an expression.
%% Simple expressions are not parenthesized.
%%
pp_pe({'string', _}=E) -> pp(E);
pp_pe({'char', _}=E) -> pp(E);
pp_pe(I) when is_integer(I) -> pp(I);
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
