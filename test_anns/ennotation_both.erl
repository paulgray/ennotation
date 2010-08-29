-module(ennotation_both).

-include("before_a_annotations.hrl").
-include("after_a_annotations.hrl").

-export([tr/1, mixed/2, stop/1, mul/3]).

?UNCHANGED(nothing).
?TRANSPARENT(nothing).
tr(MyArg) ->
    {transparent, MyArg}.

?EXCHANGE([before, annotation]).
?REVERSE(empty).
mixed(A, B) ->
    [A, B].

?STOP(stopped).
?ADD(myarg).
stop(A) ->
    erlang:throw({not_supposed_to_be_here, A}).

?EXCHANGE([1, 2, 3]).
?DUP(first).
?EXCHANGE([3, 4, 5]).
?FUN_INFO(second).
mul(A, B, C) ->
    [A, B, C].
