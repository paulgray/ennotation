-module(ennotation_after).

-include("after_a_annotations.hrl").

-export([tr/1, re/2, re/3, fu/3, ad/4]).
-export([no_change/0, two_annotations/1]).

?TRANSPARENT(something).
tr(MyArg) ->
    {MyArg, MyArg}.

?REVERSE(ugh).
re(First, Second) ->
    [1, 2, 3, First, Second].

?REVERSE(ugh).
re(First, Second, Third) ->
    {First, Second, Third}.

?FUN_INFO(foobar).
fu(A, B, C) ->
    [A, B, C].

?ADD(arg).
ad(Q, W, E, R) ->
    {Q, W, E, R}.

no_change() ->
    no_change.

?DUPLICATE(nothing).
?DUPLICATE(nothing).
two_annotations(A) ->
    A.
