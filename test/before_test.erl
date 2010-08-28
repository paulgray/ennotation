-module(before_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

transparent_test() ->
    ?assertEqual({transparent, foobar},
                 ennotation_before:tr(foobar)).

stop_test() ->
    ?assertEqual(reason,
                 ennotation_before:st(a, b)).

exchange_test() ->
    ?assertEqual({a, b, c},
                 ennotation_before:ex(1, 2, 3)).

duplicate_test() ->
    ?assertEqual({{a, a}, {b, b}}, 
                 ennotation_before:du(a, b)).
