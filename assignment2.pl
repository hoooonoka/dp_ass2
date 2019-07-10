% Question 1
correspond(A, [B|C], D, [E|F]) :-
	(A=B, D=E);
	correspond(A,C,D,F).

% Question 2: find solutions first, then filter with the same-length constraint	
interleave(Ls,L) :-
	core(Ls,L),
	samelength(Ls).

% predicate used to ensure solutions exist
core([],[]).
core([[Lhead|Lshs]|Lstail],[Lhead|Ltail]) :-
	len([[Lhead|Lshs]|Lstail],[Lhead|Ltail]),
	( Lshs \= [] ->
		append(Lstail,[Lshs],NewL),
		core(NewL,Ltail)
	; 	core(Lstail,Ltail)
	).

% predicate used to ensure the total-length constraint
len([],[]).
len([[]|Lstail],L) :-
	len(Lstail,L).
len([[Lhead|Lshs]|Lstail],[Lhead2|Ltail]) :-
	len([Lshs|Lstail],Ltail).

% predicate used to ensure the same-length constraint
samelength([]).
samelength([L|[]]).
samelength([L1,L2|Ls]) :-
	length(L1,Len1),
	length(L2,Len2),
	Len1=Len2,
	samelength([L2|Ls]).

% Question 3
% only number exists
partial_eval(Expr0, Var, Val, Expr1) :-
    number(Expr0),
    check_inputs(Var,Val),
    Expr0=Expr1.
% only atom exists
partial_eval(Expr0, Var, Val, Expr1) :-
    atom(Expr0),
    check_inputs(Var,Val),
    ( Expr0=Var ->
        Val=Expr1
    ;
        Expr0=Expr1
    ).
% for + - * / //
partial_eval(Expr0+Expr1, Var, Val, Expr2) :-
    partial_eval(Expr0, Var, Val, NewExpr0),
    partial_eval(Expr1, Var, Val, NewExpr1),
    check_inputs(Var,Val),
    ( number(NewExpr0), number(NewExpr1) ->
        Expr2 is NewExpr0+NewExpr1
    ;
        Expr2=NewExpr0+NewExpr1
    ).
partial_eval(Expr0-Expr1, Var, Val, Expr2) :-
    partial_eval(Expr0, Var, Val, NewExpr0),
    partial_eval(Expr1, Var, Val, NewExpr1),
    check_inputs(Var,Val),
    ( number(NewExpr0), number(NewExpr1) ->
        Expr2 is NewExpr0-NewExpr1
    ;
        Expr2=NewExpr0-NewExpr1
    ).
partial_eval(Expr0*Expr1, Var, Val, Expr2) :-
    partial_eval(Expr0, Var, Val, NewExpr0),
    partial_eval(Expr1, Var, Val, NewExpr1),
    check_inputs(Var,Val),
    ( number(NewExpr0), number(NewExpr1) ->
        Expr2 is NewExpr0*NewExpr1
    ;
        Expr2=NewExpr0*NewExpr1
    ).
partial_eval(Expr0/Expr1, Var, Val, Expr2) :-
    partial_eval(Expr0, Var, Val, NewExpr0),
    partial_eval(Expr1, Var, Val, NewExpr1),
    check_inputs(Var,Val),
    ( number(NewExpr0), number(NewExpr1) ->
        Expr2 is NewExpr0/NewExpr1
    ;
        Expr2=NewExpr0/NewExpr1
    ).
partial_eval(Expr0//Expr1, Var, Val, Expr2) :-
    partial_eval(Expr0, Var, Val, NewExpr0),
    partial_eval(Expr1, Var, Val, NewExpr1),
    check_inputs(Var,Val),
    ( number(NewExpr0), number(NewExpr1) ->
        Expr2 is NewExpr0//NewExpr1
    ;
        Expr2=NewExpr0//NewExpr1
    ).
% check variable and its value
check_inputs(Var,Val) :-
	number(Val),
	atom(Var).
