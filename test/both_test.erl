-module(both_test).

-include_lib("eunit/include/eunit.hrl").

transparent_test() ->
    ?assertEqual({transparent, myarg},
                 ennotation_both:tr(myarg)).

mixed_test() ->
    ?assertEqual([annotation, before],
                 ennotation_both:mixed(1, 2)).

stop_test() ->
    ?assertEqual(stopped,
                 ennotation_both:stop(foobar)).

multiple_anns_test() ->
    ?assertEqual({ennotation_both, mul, {[3, 4, 5], [3, 4, 5]}},
                 ennotation_both:mul(this, is, me)).
