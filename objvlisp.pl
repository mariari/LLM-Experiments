%%% objvlisp.pl — Metacircular Reflective Object System
%%%
%%% Four relational primitives, bidirectional via Prolog unification:
%%%
%%%   isa(Obj, Class)            instance-of
%%%   slot(Obj, Name, Value)     attribute store
%%%   inherits(Class, Super)     single inheritance
%%%   method(Class, Sel, Body)   method binding
%%%
%%% Metacircular kernel (after bootstrap/0):
%%%
%%%   isa(class, class)           class is its own metaclass
%%%   isa(object, class)          object is a class too
%%%   inherits(class, object)     class inherits from object
%%%
%%% Bidirectional queries — leave any argument unbound:
%%%
%%%   ?- slot(X, x, 3).           who has x = 3?
%%%   ?- slot(P, Name, Val).      all slots of P?
%%%   ?- isa(X, point).           instances of point?
%%%   ?- isa(Obj, What).          what class is Obj?
%%%   ?- inherits(X, object).     who extends object?
%%%   ?- ancestor(X, object).     transitive descent?

:- use_module(library(gensym)).

:- discontiguous send/1, send/2.
:- dynamic isa/2, slot/3, inherits/2, method/3.

%%% ── Bootstrap ──────────────────────────────────────────

bootstrap :-
    retractall(isa(_, _)),
    retractall(slot(_, _, _)),
    retractall(inherits(_, _)),
    retractall(method(_, _, _)),
    % metacircular fixpoint
    assertz(isa(class,  class)),
    assertz(isa(object, class)),
    assertz(inherits(class, object)),
    % classes are objects — they carry slots
    assertz(slot(class,  name,  class)),
    assertz(slot(class,  super, object)),
    assertz(slot(class,  ivars, [])),
    assertz(slot(object, name,  object)),
    assertz(slot(object, super, none)),
    assertz(slot(object, ivars, [])).

%%% ── send/1 : meta-messages ────────────────────────────

%% Instantiate from dict: send(new(point, _{x:3, y:4}, P))
send(new(Class, Init, Obj)) :-
    is_dict(Init), !,
    isa(Class, class),
    gensym(Class, Obj),
    assertz(isa(Obj, Class)),
    dict_pairs(Init, _, Pairs),
    maplist(assert_slot(Obj), Pairs).

%% Instantiate from list: send(new(point, [x=3, y=4], P))
send(new(Class, Init, Obj)) :-
    is_list(Init), !,
    isa(Class, class),
    gensym(Class, Obj),
    assertz(isa(Obj, Class)),
    maplist(assert_kv(Obj), Init).

%% Bare instance: send(new(point, P))
send(new(Class, Obj)) :-
    isa(Class, class),
    gensym(Class, Obj),
    assertz(isa(Obj, Class)).

%% Define class: send(defclass(point, object, [x, y]))
send(defclass(Name, Super, IVars)) :-
    assertz(isa(Name, class)),
    assertz(inherits(Name, Super)),
    assertz(slot(Name, name,  Name)),
    assertz(slot(Name, super, Super)),
    assertz(slot(Name, ivars, IVars)).

%% Attach method: send(defmethod(point, norm, point_norm))
send(defmethod(Class, Sel, Body)) :-
    retractall(method(Class, Sel, _)),
    assertz(method(Class, Sel, Body)).

%%% ── send/2 : object messages ──────────────────────────

%% Reflective new — message to a class creates an instance
send(Class, new(Init, Obj)) :-
    isa(Class, class), !,
    send(new(Class, Init, Obj)).
send(Class, new(Obj)) :-
    isa(Class, class), !,
    send(new(Class, Obj)).

%% Slot read  (bidirectional — mirrors slot/3 directly)
send(Obj, get(S, V)) :- slot(Obj, S, V).

%% Slot write
send(Obj, set(S, V)) :-
    retractall(slot(Obj, S, _)),
    assertz(slot(Obj, S, V)).

%% Class-of query
send(Obj, class_of(C)) :- isa(Obj, C).

%% Subtype check (walks inheritance)
send(Obj, is_a(C)) :-
    isa(Obj, Direct),
    (   Direct = C
    ;   ancestor(Direct, C)
    ).

%% User-defined method dispatch (MRO walk)
send(Obj, Msg) :-
    isa(Obj, Class),
    functor(Msg, Sel, _),
    resolve(Class, Sel, Body),
    call(Body, Obj, Msg).

%%% ── Method resolution order ───────────────────────────

resolve(C, Sel, Body) :- method(C, Sel, Body), !.
resolve(C, Sel, Body) :-
    inherits(C, Super),
    Super \= none,
    resolve(Super, Sel, Body).

%%% ── Transitive inheritance (bidirectional) ────────────

ancestor(C, A) :- inherits(C, A), A \= none.
ancestor(C, A) :- inherits(C, Mid), Mid \= none, ancestor(Mid, A).

%%% ── Query helpers ─────────────────────────────────────

instances(Class, Objs) :- findall(O, isa(O, Class), Objs).
all_slots(Obj, Pairs)  :- findall(K-V, slot(Obj, K, V), Pairs).

inspect(Obj) :-
    isa(Obj, C),
    format("~w : ~w~n", [Obj, C]),
    forall(slot(Obj, K, V), format("  ~w = ~w~n", [K, V])).

%%% ── Internal ──────────────────────────────────────────

assert_slot(Obj, K-V) :- assertz(slot(Obj, K, V)).
assert_kv(Obj, K=V)   :- assertz(slot(Obj, K, V)).

%%% ── Demo ──────────────────────────────────────────────

%% Method handler for point — computes Euclidean norm
point_norm(Obj, norm(N)) :-
    slot(Obj, x, X), slot(Obj, y, Y),
    N is sqrt(X*X + Y*Y).

%% Method handler — string representation
point_to_string(Obj, to_string(S)) :-
    slot(Obj, x, X), slot(Obj, y, Y),
    format(atom(S), "point(~w, ~w)", [X, Y]).

demo :-
    bootstrap,

    %% Define classes
    send(defclass(point, object, [x, y])),
    send(defclass(point3d, point, [z])),

    %% Attach methods
    send(defmethod(point, norm, point_norm)),
    send(defmethod(point, to_string, point_to_string)),

    %% Create instances
    send(new(point, _{x: 3, y: 4}, P)),
    send(new(point3d, [x=1, y=2, z=3], Q)),

    format("--- Objects ---~n"),
    inspect(P), nl,
    inspect(Q), nl,

    %% Message dispatch
    send(P, norm(N)),
    format("norm(P) = ~w~n", [N]),

    %% Inherited method on subclass
    send(Q, norm(NQ)),
    format("norm(Q) = ~w~n", [NQ]),

    %% Reflective instantiation: send new to the class itself
    send(point, new(_{x: 10, y: 20}, R)),
    format("~nReflective new: "), inspect(R),

    %% Bidirectional queries
    format("~n--- Bidirectional Queries ---~n"),
    format("slot(X, x, 3):       "),
    findall(X, slot(X, x, 3), Xs), write(Xs), nl,

    format("isa(X, point):       "),
    findall(X, isa(X, point), Ps), write(Ps), nl,

    format("isa(X, class):       "),
    findall(X, isa(X, class), Cs), write(Cs), nl,

    format("inherits(X, object): "),
    findall(X, inherits(X, object), Hs), write(Hs), nl,

    format("ancestor(point3d,X): "),
    findall(X, ancestor(point3d, X), As), write(As), nl,

    %% Metacircular check
    format("~n--- Metacircular ---~n"),
    (send(class, is_a(object)) -> format("class is_a object: true~n") ; true),
    (send(class, class_of(class)) -> format("class class_of class: true~n") ; true),
    send(point, class_of(PC)),
    format("point class_of: ~w~n", [PC]).
