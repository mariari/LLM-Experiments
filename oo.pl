%% Minimal object system

:- discontiguous class/2, has/3, oapply/2, super/2.

%% Base ontology
class(object, class).
class(class, class).
super(class, object).

%% Method dispatch
send(Method) :-
    Method =.. [Name, Self | Args],
    class(Self, Class),
    lookup(Class, Name, Impl),
    oapply(Impl, [Self | Args]).

lookup(Class, Name, Impl) :-
    has(Class, Name, Impl).
lookup(Class, Name, Impl) :-
    super(Class, Super),
    lookup(Super, Name, Impl).

%% new: allocate then initialize
has(class, new, new_impl).
class(new_impl, behaviour).
oapply(new_impl, [Self, Instance]) :-
    send(allocate(Self, Instance)),
    send(initialize(Instance)).

%% allocate: create a bare instance
has(class, allocate, allocate_impl).
class(allocate_impl, behaviour).
oapply(allocate_impl, [Self, Instance]) :-
    assert(class(Instance, Self)).

%% initialize: default no-op
has(object, initialize, initialize_impl).
class(initialize_impl, behaviour).
oapply(initialize_impl, [_Self]).
