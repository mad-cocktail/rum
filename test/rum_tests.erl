-module(rum_tests).

-compile({parse_transform, rum}).
-compile(export_all).


-record(rec, {f1 = 1, f2 = 2}).

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


nested_test_() ->
    [ ?_assertEqual(nested(),  #rec{f1 = #rec{f1 = f1y, f2 = f2y}, f2 = f2x})
    , ?_assertEqual(nested2(), #rec{f1 = #rec{f1 = f1y, f2 = f1x}, f2 = f2x})].


update_with_fun_test_() ->
    [ ?_assertEqual(update_with_fun(),  #rec{f1 = "f1x", f2 = "f2x"})
    ].

-endif.
