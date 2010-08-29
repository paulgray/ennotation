-module(ennotation_before).

-include("before_a_annotations.hrl").

-export([tr/1, st/2, du/2, ex/3]).
-export([no_change/0, two_anns/2, wrong/3]).

?UNCHANGED(something).
tr(MyArg) ->
    {transparent, MyArg}.

?STOP(reason).
st(A, B) ->
    {A, B}.

?EXCHANGE([a, b, c]).
ex(A, B, C) ->
    {A, B, C}.

?DUPLICATE(noop).
du(A, B) ->
    {A, B}.

no_change() ->
    no_change.

?APPEND(first).
?APPEND(second).
two_anns(List, _) ->
    {length(List), List}.

?INCOMPATIBLE(wrong).
wrong(A, B, C) ->
    {A, B, C}.
