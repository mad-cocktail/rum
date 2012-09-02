-module(rum_tests).

-compile({parse_transform, rum}).
-compile(export_all).


-record(rec, {f1 = 1, f2 = 2}).
-record(rec2, {f1 = erlang:error(empty_field), f2 = 2}).

%% Return default value.
before_example1() ->
    element(#rec.f1, #rec{}).

after_example1() ->
    default(#rec.f1).

%% Is default value?
before_example2(X) ->
    X#rec.f1 =:= element(#rec.f1, #rec{}).

after_example2(X) ->
    is_default(X#rec.f1).

%% Update record.
before_example3(X) ->
    X#rec{f2 = element(#rec.f2, #rec{})}.

after_example3(X) ->
    X#rec{f2 = default()}.


before_example4(X) ->
    X#rec{f1 = X#rec.f1 + 1}.

after_example4(X) ->
    X#rec{f1 = old() + 1}.


before_example5(X) ->
    X#rec{f1 = X#rec.f1 + 1, f2 = element(#rec.f2, #rec{})}.

after_example5(X) ->
    X#rec{f1 = old() + 1, f2 = default()}.


before_example6(X) ->
    X#rec{f2 = element(#rec.f2, #rec{}) + 1}.

after_example6(X) ->
    X#rec{f2 = default() + 1}.


before_example7(X) when X#rec.f1 =:= element(#rec.f1, #rec{}) ->
    true;
before_example7(_X) ->
    false.


after_example7(X) when is_default(X#rec.f1) ->
    true;
after_example7(_X) ->
    false.


nested() ->
    X = #rec{f1 = f1x, f2 = f2x},
    Y = #rec{f1 = f1y, f2 = f2y},
    %% `old()' is `f1y' (the value of the record Y, but not X), 
    %% because of `postprocess'.
    X#rec{f1 = Y#rec{f2 = old()}}.
    

nested2() ->
    X = #rec{f1 = f1x, f2 = f2x},
    Y = #rec{f1 = f1y, f2 = f2y},
    %% Get the old value of the record X.
    X#rec{f1 = Y#rec{f2 = old(X)}}.


update_with_fun() ->
    X = #rec{f1 = f1x, f2 = f2x},
    X#rec{f1 = atom_to_list(old()), f2 = atom_to_list(old())}.


%% Parse transform inserts a default value.
%% Other fields are not constructed, and the field constructors are ignored.
ignore_other_fields(X) when is_default(X#rec2.f2) ->
    true;
ignore_other_fields(_) ->
    false.


recbind_before(X) ->
    X#rec{f1 = X#rec.f1#rec2{f1 = 2}}.

recbind(X) ->
    X#rec{f1 = with(Y, Y#rec2{f1 = 2})}.

-record(a, {a}).
-record(b, {b}).
-record(c, {c}).

ab_set_before(A, X) ->
    A#a{a = A#a.a#b{b = X}}.

ab_set(A, X) ->
    A#a{a = with(B, 
                 B#b{b = X})}.

abc_set(A, X) ->
    A#a{a = with(B, 
                 B#b{b = with(C, 
                              C#c{c = X})})}.

abc_set_before(A, X) ->       
    A#a{a = A#a.a      
     #b{b = A#a.a#b.b  
     #c{c = X}}}.      


-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

compare_test_() ->
    X = #rec{},
    [ ?_assertEqual(before_example1(), after_example1())
    , ?_assertEqual(before_example2(X), after_example2(X))
    , ?_assertEqual(before_example3(X), after_example3(X))
    , ?_assertEqual(before_example4(X), after_example4(X))
    , ?_assertEqual(before_example5(X), after_example5(X))
    , ?_assertEqual(before_example6(X), after_example6(X))
    , ?_assertEqual(before_example7(X), after_example7(X))
    ].

ab_set_test_() ->
    A1 = #a{a = #b{b = 1}},
    A2 = #a{a = #b{b = 2}},
    [ ?_assertEqual(ab_set(A1, 2), A2)].

abc_set_test_() ->
    A1 = #a{a = #b{b = #c{c = 1}}},
    A2 = #a{a = #b{b = #c{c = 2}}},
    [ ?_assertEqual(abc_set(A1, 2), A2)
    , ?_assertEqual(abc_set_before(A1, 2), A2)].

recbind_test_() ->
    X = #rec{f1 = #rec2{f1 = 3}},
    [ ?_assertEqual(recbind(X), #rec{f1 = #rec2{f1 = 2}})].


nested_test_() ->
    [ ?_assertEqual(nested(),  #rec{f1 = #rec{f1 = f1y, f2 = f2y}, f2 = f2x})
    , ?_assertEqual(nested2(), #rec{f1 = #rec{f1 = f1y, f2 = f1x}, f2 = f2x})].


update_with_fun_test_() ->
    [ ?_assertEqual(update_with_fun(),  #rec{f1 = "f1x", f2 = "f2x"})
    ].

ignore_other_fields_test_() ->
    [ ?_assert(ignore_other_fields(#rec2{f1 = undefined}))
    , ?_assertNot(ignore_other_fields(#rec2{f1 = undefined, f2 = other}))
    , ?_assertNot(ignore_other_fields(#rec{}))
    , ?_assertNot(ignore_other_fields(#rec{f2 = default(#rec2.f2)}))
    , ?_assert(ignore_other_fields(#rec2{f1 = default(#rec2.f2)}))
    ].

-endif.
