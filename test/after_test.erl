-module(after_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

transparent_test() ->
    ?assertEqual({foobar, foobar}, 
                 ennotation_after:tr(foobar)).
reverse_test() ->
    ?assertEqual([bar, foo, 3, 2, 1], 
                 ennotation_after:re(foo, bar)),
    ?assertEqual({no, change, here},
                 ennotation_after:re(no, change, here)).

fun_info_test() ->
    ?assertEqual({ennotation_after, fu, [1, 2, 3]}, 
                 ennotation_after:fu(1, 2, 3)).

add_test() ->
    ?assertEqual(arg,
                 ennotation_after:ad(a, b, c, d)).
