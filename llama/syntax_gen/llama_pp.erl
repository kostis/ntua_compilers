%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%%
%%% @doc A (not so pretty) pretty-printer of Llama programs.
%%%
%%% <p>It exports three functions:
%%%   1. pp/1, which returns an IO list of the string of a Llama program
%%%   2. p/0, which prints a Llama program on stdout
%%%   3. gen/1, which generates N programs under a `llamas' directory
%%% The first of these functions is general; the others have a hard-coded
%%% call to the generator of syntactically correct Llama programs.  This
%%% needs to be made more general, at some point.</p>
%%%
%%% TODO: Some expressions may still need to be enclosed in parentheses.
%%%
%%% -------------------------------------------------------------------
-module(llama_pp).

-export([gen/1, p/0, pp/1]).

-define(F(S, L), io_lib:format(S, L)).

-spec gen(non_neg_integer()) -> 'ok'.

gen(0) -> ok;
gen(N) ->
  FN = "llamas/p"++integer_to_list(N)++".lla",
  {ok, P} = proper_gen:pick(llama_syntax_gen:program()),
  ok = file:write_file(FN, pp(P)),
  gen(N - 1).

-spec p() -> 'ok'.

p() ->
  {ok, P} = proper_gen:pick(llama_syntax_gen:program()),
  io:format(pp(P)).

-spec pp(_) -> iolist().  % XXX: define a type for Llama abstract syntax

pp([]) -> "";
pp([H|T]) -> ?F("~s~n~n~s", [pp(H), pp(T)]);
pp({'let', Defs}) -> ?F("let ~s", [pp_defs(Defs, "and")]);
pp({'letrec', Defs}) -> ?F("let rec ~s", [pp_defs(Defs, "and")]);
pp({'type', Defs}) -> ?F("type ~s", [pp_defs(Defs, " and")]);
pp({'paren', E}) -> ?F("(~s)", [pp(E)]);
pp({'app', Id, Es}) -> ?F("~s~s", [pp(Id), pp_pars(Es)]);
pp({'arr', Id, Es}) -> ?F("~s~s", [pp(Id), pp_arrs(Es)]);
pp({'dim', N, Id}) -> ?F("dim~s ~s", [pp_dim(N), pp(Id)]);
pp({'new', T}) -> ?F("new ~s", [pp_type(T)]);
pp({'delete', E}) -> ?F("delete ~s", [pp_pe(E)]);
pp({'letdef', LD, E}) -> ?F("~s in ~s", [pp(LD), pp_pe(E)]);
pp({'begin_end', E}) -> ?F("begin ~s end", [pp(E)]); % no parentheses are needed
pp({'if', C, T, '()'}) -> ?F("if ~s then ~s", [pp_pe(C), pp(T)]);
pp({'if', C, T, E}) -> ?F("if ~s then ~s else ~s", [pp_pe(C), pp_pe(T), pp(E)]);
pp({'while', E1, E2}) -> ?F("while ~s do ~s done", [pp(E1), pp(E2)]);
pp({'for', Id, E1, Dir, E2, E3}) ->
  ?F("for ~s = ~s ~s ~s do ~s done", [pp(Id), pp(E1), Dir, pp(E2), pp(E3)]);
pp({'match', E, Cls}) ->
  ?F("match ~s with~n ~send~n", [pp(E), pp_clauses(Cls)]);
pp({'unop', Op, E}) -> ?F("~s ~s", [pp(Op), pp_pe(E)]);
pp({'binop', Op, E1, E2}) -> ?F("(~s ~s ~s)", [pp(E1), pp_pe(Op), pp_pe(E2)]);
pp({'string', S}) -> ?F("~p", [S]);
pp({'char', C}) -> pp_char(C);
pp('()') -> "()";
pp(N) when is_number(N) -> ?F("~p", [N]);
pp(A) when is_atom(A) -> ?F("~s", [A]).

pp_defs([D], _) -> ?F("~s", [pp_def(D)]);
pp_defs([D|Ds], AND) -> ?F("~s~n~s ~s", [pp_def(D), AND, pp_defs(Ds, AND)]).

pp_def({'def', Id, Ps, T, E}) ->
  ?F("~s~s~s = ~s", [Id, pp_pars(Ps), pp_opt_type(T), pp(E)]);
pp_def({'mutable', Id, Es, T}) ->
  ?F("mutable ~s~s~s", [Id, pp_mutexprs(Es), pp_opt_type(T)]);
pp_def({'tdef', Id, Cs}) -> ?F("~s = ~s", [pp(Id), pp_constrs(Cs)]).

pp_mutexprs([]) -> "";
pp_mutexprs(Es) -> ?F("[~s]", [pp_mutexprs_aux(Es)]).

pp_mutexprs_aux([E]) -> ?F("~s", [pp(E)]);
pp_mutexprs_aux([E|Es]) -> ?F("~s,~s", [pp(E), pp_mutexprs_aux(Es)]).

pp_constrs([C]) -> pp_constr(C);
pp_constrs([C|Cs]) -> ?F("~s | ~s", [pp_constr(C), pp_constrs(Cs)]). 

pp_constr({'constr', ID}) -> pp(ID);
pp_constr({'constr', ID, Types}) -> ?F("~s of ~s", [pp(ID), pp_types(Types)]).

pp_types([T]) -> pp_type(T);
pp_types([T|Ts]) -> ?F("~s ~s", [pp_type(T), pp_types(Ts)]).

pp_opt_type('') -> "";
pp_opt_type(T) -> ?F(" : ~s", [pp_type(T)]).

pp_pars([]) -> "";
pp_pars([P]) -> ?F(" ~s", [pp_par(P)]);
pp_pars([P|Ps]) -> ?F(" ~s~s", [pp_par(P), pp_pars(Ps)]).

pp_par({'paren', Id, T}) -> ?F("(~s : ~s)", [pp(Id), pp_type(T)]);
pp_par(Id) -> pp_pe(Id).

pp_clauses([Cl]) -> ?F(" ~s~n", [pp_clause(Cl)]);
pp_clauses([Cl|Cls]) -> ?F(" ~s~n|~s", [pp_clause(Cl), pp_clauses(Cls)]).

pp_clause({'clause', Patt, E}) ->
  ?F("~s -> ~s", [pp_pattern(Patt), pp(E)]).

pp_pattern({'+', Int}) when is_integer(Int) -> ?F("(+~s)", [pp(Int)]);
pp_pattern({'-', Int}) when is_integer(Int) -> ?F("(-~s)", [pp(Int)]);
pp_pattern({'+.', Fl}) when is_float(Fl) -> ?F("(+.~s)", [pp(Fl)]);
pp_pattern({'-.', Fl}) when is_float(Fl) -> ?F("(-.~s)", [pp(Fl)]);
pp_pattern({'char', Char}) -> pp_char(Char);
pp_pattern({'paren', Patt}) -> ?F("(~s)", [pp_pattern(Patt)]);
pp_pattern({ID, Patts}) -> ?F("~s~s", [pp(ID), pp_patterns(Patts)]);
pp_pattern(Patt) -> pp_pe(Patt).

pp_patterns([]) -> "";
pp_patterns([P|Ps]) -> ?F(" ~s~s", [pp_pattern(P), pp_patterns(Ps)]).

%%
%% Exists only for determining when to put parentheses around an expression.
%% Simple expressions are not parenthesized.
%%
pp_pe({'string', _}=E) -> pp(E);
pp_pe({'char', _}=E) -> pp(E);
pp_pe('()'=E) -> pp(E);
pp_pe(N) when is_number(N) -> pp(N);
pp_pe(A) when is_atom(A) -> pp(A);
pp_pe(E) -> ?F("(~s)", [pp(E)]).

pp_type({'paren', T}) -> ?F("(~s)", [pp_type(T)]);
pp_type({'arrow', T1, T2}) -> ?F("~s -> ~s", [pp_type(T1), pp_type(T2)]);
pp_type({'ref', T}) -> ?F("~s ref", [pp_type(T)]);
pp_type({'array', S, T}) -> ?F("array~s of ~s", [pp_stars(S), pp_type(T)]);
pp_type(T) -> pp(T).

%%
%% The following functions depend on MAX_ARR_DIM being 2.
%%
pp_stars(unknown) -> "";
pp_stars(1) -> " [*]";
pp_stars(2) -> " [*,*]".

pp_arrs([E1]) -> ?F("[~s]", [pp(E1)]);
pp_arrs([E1,E2]) -> ?F("[~s,~s]", [pp(E1), pp(E2)]).

pp_dim(unknown) -> "";
pp_dim(1) -> " 1"; 
pp_dim(2) -> " 2".

%%
%% The pretty-printing of chars is hard-coded
%% (due to Erlang printing chars differently),
%%
pp_char('a') -> "'a'";
pp_char('7') -> "'7'";
pp_char('\n') -> "'\\n'";
pp_char('\'') -> "'\\''".
