-module(after_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

transparent_test() ->
    ?assertEqual({foobar, foobar}, 
                 ennotation_after:tr(foobar)).
