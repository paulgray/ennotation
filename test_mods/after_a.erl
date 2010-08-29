-module(after_a).

-include("ennotation.hrl").

-export([transparent/4,
         reverse/4,
         fun_info/4,
         add/4,
         dup/4]).

?AFTER.
transparent(_, _, _, Result) ->
    Result.

?AFTER.
reverse(_, _, _, Result) ->
    if
        is_list(Result) ->
            lists:reverse(Result);
        true ->
            Result
    end.

?AFTER.
fun_info(_, Mod, Fun, Res) ->
    {Mod, Fun, Res}.

?AFTER.
add(AArgs, _, _, _) ->
    AArgs.

?AFTER.
dup(_, _, _, Res) ->
    {Res, Res}.
