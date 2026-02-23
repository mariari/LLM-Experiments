%%% objvlisp.pl - Reflective Object System à la ObjVLisp (Cointe, 1987)
%%% Two axioms: Class is-instance-of Class. Class is-subclass-of Object.

:- dynamic obj/3, method/5.

%% obj(Id, Class, Slots)                     - object store
%% method(Class, Sel, Self, Args, Result)     - methods are just clauses

%%% ---- Core MOP ----

get(O, K, V)     :- obj(O, _, S), member(K=V, S).
resolve(C, M, C) :- clause(method(C, M, _, _, _), _), !.
resolve(C, M, W) :- get(C, super, P), P \= none, resolve(P, M, W).
send(O, M, A, R) :- obj(O, C, _), resolve(C, M, W), method(W, M, O, A, R).
send(O, M, A)    :- send(O, M, A, _).
send(O, M)       :- send(O, M, []).

%%% ---- Bootstrap: the metacircular kernel ----

:- assertz(obj(object, class, [name=object, super=none, ivars=[]])).
:- assertz(obj(class,  class, [name=class,  super=object, ivars=[name,super,ivars]])).

method(class, new, Cls, [Id, Slots], Id) :- assertz(obj(Id, Cls, Slots)).

%%% ---- Convenience ----

set(O, K, V) :-
    retract(obj(O, C, S)),
    (select(K=_, S, R) -> true ; R = S),
    assertz(obj(O, C, [K=V|R])).

class_of(O, C)      :- obj(O, C, _).
def_class(N, S, IV) :- send(class, new, [N, [name=N, super=S, ivars=IV]]).

%%% ---- Example usage ----
%%%
%%% ?- def_class(point, object, [x, y]).
%%%
%%% % Methods are plain Prolog clauses:
%%% method(point, show, Self, [], ok) :-
%%%     get(Self, x, X), get(Self, y, Y),
%%%     format("(~w, ~w)~n", [X, Y]).
%%%
%%% method(object, identity, Self, [], Self).
%%%
%%% ?- send(point, new, [p1, [x=3, y=4]]).
%%% ?- send(p1, show).           % prints (3, 4)
%%% ?- send(p1, identity, [], R). % R = p1  (inherited from object)
%%% ?- set(p1, x, 10), send(p1, show).  % prints (10, 4)
%%%
%%% Metacircular check:
%%% ?- class_of(class, class).    % true - Class is instance of Class
%%% ?- class_of(object, class).   % true - Object is instance of Class
%%% ?- get(class, super, object). % true - Class inherits from Object
