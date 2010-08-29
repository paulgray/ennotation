-module(before_a).

-include("ennotation.hrl").

-export([unchanged/4,
         stop/4,
         exchange/4,
         append/4,
         duplicate/4]).

?BEFORE.
unchanged(_, _, _, Args) ->
    {ok, Args}.

?BEFORE.
stop(Args, _, _, _) ->
    {stop, Args}.

?BEFORE.
exchange(Args, _, _, _) ->
    {ok, Args}.

?BEFORE.
duplicate(_, _, _, Args) ->
    {ok, [{Arg, Arg} || Arg <- Args]}.

?BEFORE.
append(Arg, _, _, [List | Rest]) when is_list(List) ->
    {ok, [[Arg | List] | Rest]};
append(_, _, _, Args) ->
    {ok, Args}.
