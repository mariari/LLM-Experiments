%%%=========================================================================
%%% test_kernel.pl — Tests for the reflective object kernel
%%%=========================================================================

:- [object_kernel].
:- use_module(library(plunit)).

%%% ---- Set up some classes ----

:- defclass(point, object, [x, y]).
:- defclass(point3d, point, [z]).
:- defclass(color, object, [r, g, b]).

%%% ---- Custom methods ----

:- defmethod(point, distance, [Self, D], (
       send(x(Self, X)),
       send(y(Self, Y)),
       D is sqrt(X*X + Y*Y)
   )).

:- defmethod(point, add, [Self, Other, Result], (
       send(x(Self, X1)), send(y(Self, Y1)),
       send(x(Other, X2)), send(y(Other, Y2)),
       X is X1 + X2, Y is Y1 + Y2,
       send(new(point, #{x: X, y: Y}, Result))
   )).

:- defmethod(point3d, distance, [Self, D], (
       send(x(Self, X)),
       send(y(Self, Y)),
       send(z(Self, Z)),
       D is sqrt(X*X + Y*Y + Z*Z)
   )).

%%% ---- Tests ----

:- begin_tests(basics).

test(create_point, [nondet]) :-
    send(new(point, #{x: 3, y: 4}, P)),
    send(x(P, 3)),
    send(y(P, 4)).

test(create_no_args, [nondet]) :-
    send(new(point, P)),
    send(class_of(P, point)).

test(distance, [nondet]) :-
    send(new(point, #{x: 3, y: 4}, P)),
    send(distance(P, D)),
    D =:= 5.0.

:- end_tests(basics).

:- begin_tests(inheritance).

test(point3d_inherits_x_y, [nondet]) :-
    send(new(point3d, #{x: 1, y: 2, z: 3}, P)),
    send(x(P, 1)),
    send(y(P, 2)),
    send(z(P, 3)).

test(point3d_overrides_distance, [nondet]) :-
    send(new(point3d, #{x: 1, y: 2, z: 2}, P)),
    send(distance(P, D)),
    D =:= 3.0.

test(superclass_chain, [nondet]) :-
    send(superclass(point3d, point)),
    send(superclass(point, object)).

:- end_tests(inheritance).

:- begin_tests(bidirectional).

test(find_by_slot_value, [nondet]) :-
    send(new(point, #{x: 42, y: 0}, _)),
    send(new(point, #{x: 42, y: 1}, _)),
    findall(P, send(x(P, 42)), Ps),
    length(Ps, Len),
    Len >= 2.

test(query_class, [nondet]) :-
    send(new(color, #{r: 255, g: 0, b: 0}, C)),
    send(class_of(C, color)).

:- end_tests(bidirectional).

:- begin_tests(mutation).

test(set_slot, [nondet]) :-
    send(new(point, #{x: 0, y: 0}, P)),
    send(set_slot(P, x, 99)),
    send(x(P, 99)).

:- end_tests(mutation).

:- begin_tests(reflection).

test(responds_to, [nondet]) :-
    send(new(point, #{x: 0, y: 0}, P)),
    send(responds_to(P, x)),
    send(responds_to(P, distance)),
    send(responds_to(P, initialize)).

test(methods_list, [nondet]) :-
    send(methods(point, Ms)),
    memberchk(x, Ms),
    memberchk(y, Ms).

test(methods_are_objects, [nondet]) :-
    method_on(point, x, Mid),
    send(class_of(Mid, method)),
    send(get_slot(Mid, name, x)).

test(slots_introspection, [nondet]) :-
    send(new(point, #{x: 7, y: 8}, P)),
    send(slots(P, Pairs)),
    memberchk(x-7, Pairs),
    memberchk(y-8, Pairs).

test(class_is_instance_of_itself, [nondet]) :-
    send(class_of(class, class)).

test(object_is_instance_of_class, [nondet]) :-
    send(class_of(object, class)).

:- end_tests(reflection).

:- begin_tests(method_objects).

test(add_method, [nondet]) :-
    send(new(point, #{x: 1, y: 2}, A)),
    send(new(point, #{x: 3, y: 4}, B)),
    send(add(A, B, C)),
    send(x(C, 4)),
    send(y(C, 6)).

:- end_tests(method_objects).

:- begin_tests(class_via_message).

test(create_class_via_new, [nondet]) :-
    send(new(class, #{name: animal, superclass: object, slots: [species, legs]}, Cls)),
    send(new(Cls, #{species: cat, legs: 4}, A)),
    send(species(A, cat)),
    send(legs(A, 4)).

:- end_tests(class_via_message).
