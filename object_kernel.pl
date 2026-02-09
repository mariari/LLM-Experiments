%%%=========================================================================
%%% object_kernel.pl — Reflective Object Kernel (OBJVLisp-style)
%%%=========================================================================
%%%
%%% A metacircular object system built on Prolog relations.
%%%
%%%   - Objects, classes, and methods are all first-class objects
%%%   - State stored as Prolog facts: isa/2, slot/3, inherits/2
%%%   - Message dispatch via send/1 with inheritance-based lookup
%%%   - Bidirectional querying via Prolog unification/backtracking
%%%
%%% Bootstrap (only two classes needed):
%%%   object — root of all objects
%%%   class  — metaclass (instance of itself, inherits from object)
%%%
%%% Requires SWI-Prolog 8.0+ (dict syntax).
%%%=========================================================================

:- use_module(library(apply), [maplist/2]).
:- use_module(library(gensym), [gensym/2]).

:- discontiguous isa/2, slot/3, inherits/2, method_on/3.
:- dynamic isa/2, slot/3, inherits/2, method_on/3, next_oid/1.

next_oid(0).

%%%=========================================================================
%%% OBJECT IDENTITY
%%%=========================================================================

fresh_oid(oid(N)) :-
    retract(next_oid(N)),
    succ(N, N1),
    assertz(next_oid(N1)).

%%%=========================================================================
%%% MESSAGE DISPATCH
%%%=========================================================================

%% send(+Message)
%%
%% Message is a compound term:  Selector(Receiver, Arg1, ..., ArgN)
%%
%% The functor is the selector, the first argument is the receiver.
%% Unbound arguments enable bidirectional querying:
%%
%%   send(x(P, 3))          — find any P whose x is 3
%%   send(x(SomePoint, X))  — get x of SomePoint into X

send(Message) :-
    Message =.. [Selector, Receiver | _],
    class_of(Receiver, Class),
    resolve(Class, Selector, MethodObj),
    slot(MethodObj, body, Body),
    call(Body, Message).

%%%=========================================================================
%%% CLASS + METHOD LOOKUP
%%%=========================================================================

%% class_of(?Obj, ?Class) — relational, bidirectional.
class_of(Obj, Class) :-
    isa(Obj, Class).

%% resolve(+Class, +Selector, -MethodObj)
%% Walk the inheritance chain; committed choice on first match.
resolve(Class, Selector, MethodObj) :-
    method_on(Class, Selector, MethodObj), !.
resolve(Class, Selector, MethodObj) :-
    inherits(Class, Super),
    resolve(Super, Selector, MethodObj).

%%%=========================================================================
%%% BOOTSTRAP — METHOD IMPLEMENTATIONS
%%%=========================================================================

%%% -- new --
%%% Sent to a class.  Allocates, then initializes.
%%%   send(new(ClassName, InitDict, Result))
%%%   send(new(ClassName, Result))            — no init args

impl_new(new(Class, InitArgs, Result)) :- !,
    send(allocate(Class, Obj)),
    send(initialize(Obj, InitArgs)),
    Result = Obj.
impl_new(new(Class, Result)) :-
    send(allocate(Class, Obj)),
    Result = Obj.

%%% -- allocate --
%%% Creates a raw instance with a fresh oid.

impl_allocate(allocate(Class, Obj)) :-
    fresh_oid(Obj),
    assertz(isa(Obj, Class)).

%%% -- initialize  (on object) --
%%% Default: populate slots from a dict.

impl_initialize(initialize(_)) :- !.
impl_initialize(initialize(Obj, Init)) :-
    is_dict(Init), !,
    dict_pairs(Init, _, Pairs),
    maplist(assert_slot_pair(Obj), Pairs).
impl_initialize(initialize(_, _)).

assert_slot_pair(Obj, K-V) :-
    assertz(slot(Obj, K, V)).

%%% -- initialize  (on class) --
%%% When the new object IS a class, wire up inheritance + accessors.

impl_class_init(initialize(Cls, Init)) :-
    is_dict(Init), !,
    ( get_dict(name, Init, Name)
    -> assertz(slot(Cls, name, Name))
    ;  true
    ),
    ( get_dict(superclass, Init, Sup)
    -> assertz(inherits(Cls, Sup)),
       assertz(slot(Cls, superclass, Sup))
    ;  true
    ),
    ( get_dict(slots, Init, Slots)
    -> assertz(slot(Cls, slot_names, Slots)),
       maplist(create_accessor(Cls), Slots)
    ;  true
    ).
impl_class_init(initialize(_, _)).

%%% -- get_slot / set_slot --

impl_get_slot(get_slot(Obj, Name, Val)) :-
    slot(Obj, Name, Val).

impl_set_slot(set_slot(Obj, Name, Val)) :-
    ( retract(slot(Obj, Name, _)) -> true ; true ),
    assertz(slot(Obj, Name, Val)).

%%% -- Reflective methods --

impl_class_of(class_of(Obj, C))    :- class_of(Obj, C).
impl_responds(responds_to(Obj, S)) :- class_of(Obj, C), resolve(C, S, _).
impl_methods(methods(Cls, Ms))     :- findall(S, method_on(Cls, S, _), Ms).
impl_super(superclass(Cls, Sup))   :- inherits(Cls, Sup).
impl_slots(slots(Obj, Pairs))     :- findall(K-V, slot(Obj, K, V), Pairs).

%%%=========================================================================
%%% BOOTSTRAP — WIRING
%%%=========================================================================

%% Core classes — object and class are enough to bootstrap everything.
%% class is an instance of itself; object is an instance of class.
%% class inherits from object.  method inherits from object.

:- assertz(isa(object, class)).
:- assertz(isa(class,  class)).
:- assertz(isa(method, class)).

:- assertz(inherits(class,  object)).
:- assertz(inherits(method, object)).

:- assertz(slot(object, name, object)).
:- assertz(slot(class,  name, class)).
:- assertz(slot(method, name, method)).

%% Bootstrap method objects.
%% Each is an instance of 'method' with name + body slots.

boot_method(Id, Selector, OnClass, Impl) :-
    assertz(isa(Id, method)),
    assertz(slot(Id, name, Selector)),
    assertz(slot(Id, body, Impl)),
    assertz(method_on(OnClass, Selector, Id)).

:- boot_method(boot_new,        new,         class,  impl_new).
:- boot_method(boot_alloc,      allocate,    class,  impl_allocate).
:- boot_method(boot_init,       initialize,  object, impl_initialize).
:- boot_method(boot_class_init, initialize,  class,  impl_class_init).
:- boot_method(boot_get,        get_slot,    object, impl_get_slot).
:- boot_method(boot_set,        set_slot,    object, impl_set_slot).
:- boot_method(boot_classof,    class_of,    object, impl_class_of).
:- boot_method(boot_responds,   responds_to, object, impl_responds).
:- boot_method(boot_methods,    methods,     class,  impl_methods).
:- boot_method(boot_super,      superclass,  class,  impl_super).
:- boot_method(boot_slots,      slots,       object, impl_slots).

%%%=========================================================================
%%% CONVENIENCE — defclass/3
%%%=========================================================================

%% defclass(+Name, +Super, +SlotNames)
%%
%% Define a named class (atom).  Auto-generates slot accessor methods
%% that support bidirectional unification.
%%
%%   :- defclass(point, object, [x, y]).
%%   ?- send(new(point, #{x: 3, y: 4}, P)), send(x(P, X)).

defclass(Name, Super, Slots) :-
    assertz(isa(Name, class)),
    assertz(inherits(Name, Super)),
    assertz(slot(Name, name, Name)),
    assertz(slot(Name, superclass, Super)),
    assertz(slot(Name, slot_names, Slots)),
    maplist(create_accessor(Name), Slots).

%% create_accessor(+Class, +SlotName)
%% Generates a method object whose body reads/unifies via slot/3.

create_accessor(Class, SlotName) :-
    gensym(accessor_, PredName),
    Msg  =.. [SlotName, Self, Value],
    Head =.. [PredName, Msg],
    assertz((Head :- slot(Self, SlotName, Value))),
    fresh_oid(Mid),
    assertz(isa(Mid, method)),
    assertz(slot(Mid, name, SlotName)),
    assertz(slot(Mid, class, Class)),
    assertz(slot(Mid, body, PredName)),
    assertz(method_on(Class, SlotName, Mid)).

%%%=========================================================================
%%% CONVENIENCE — defmethod/3, defmethod/4
%%%=========================================================================

%% defmethod(+Class, +Selector, +BodyPredicate)
%%
%% Attach an existing predicate as a method.  The predicate receives the
%% full message term:  my_pred(selector(Receiver, Arg1, ...)).

defmethod(Class, Selector, BodyPred) :-
    fresh_oid(Mid),
    assertz(isa(Mid, method)),
    assertz(slot(Mid, name, Selector)),
    assertz(slot(Mid, class, Class)),
    assertz(slot(Mid, body, BodyPred)),
    ( retract(method_on(Class, Selector, _)) -> true ; true ),
    assertz(method_on(Class, Selector, Mid)).

%% defmethod(+Class, +Selector, +Args, +Goal)
%%
%% Define a method with an inline body.  Args is a list of variables
%% that map to the message arguments.  Goal uses those same variables.
%%
%%   :- defmethod(point, dist, [Self, D], (
%%          send(x(Self, X)),
%%          send(y(Self, Y)),
%%          D is sqrt(X*X + Y*Y)
%%      )).

defmethod(Class, Selector, Args, Goal) :-
    gensym(method_, PredName),
    Msg  =.. [Selector | Args],
    Head =.. [PredName, Msg],
    assertz((Head :- Goal)),
    defmethod(Class, Selector, PredName).
