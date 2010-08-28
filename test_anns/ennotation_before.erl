-module(ennotation_before).

-include("before_a_annotations.hrl").

-export([tr/1, st/2, du/2, ex/3]).

?UNCHANGED(something).
tr(MyArg) ->
    case lists:seq(1, 10) of
        {ok, [1, 2, 3]} ->
            something1;
        {stop, Reason} ->
            something2;
        _ ->
            {transparent, MyArg}
    end.

?STOP(reason).
st(A, B) ->
    {A, B}.

?EXCHANGE([a, b, c]).
ex(A, B, C) ->
    {A, B, C}.

?DUPLICATE(noop).
du(A, B) ->
    {A, B}.
