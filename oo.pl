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

%% 'new' method on class
has(class, new, new_object).
class(new_object, behaviour).
oapply(new_object, [Self, Instance]) :-
    assert(class(Instance, Self)).
