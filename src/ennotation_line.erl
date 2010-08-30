%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc Module responsible for fixing line numbers after annotation transformations.
%%% @end
%%% Created : 29 Aug 2010 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ennotation_line).

-export([correct_line_numbers/3]).

-spec(correct_line_numbers/3 :: (list(), integer(), list()) -> list()).
correct_line_numbers([{function, {added, Offset}, Name, Arity, Clauses} | Rest], 
                     Current, Acc) ->
    NewClauses = correct_line_numbers(Clauses, Current, []),
    correct_line_numbers(Rest, Current, 
                         [{function, Current + Offset, Name, Arity, NewClauses} | Acc]);
correct_line_numbers([{clause, {added, Offset}, Args, Guards, Body} | Rest], 
                     Current, Acc) ->
    NewBody = correct_line_numbers(Body, Current, []),
    NewArgs = correct_line_numbers(Args, Current, []),
    correct_line_numbers(Rest, Current, 
                         [{clause, Current + Offset, NewArgs, Guards, NewBody} | Acc]);
correct_line_numbers([{match, {added, Offset}, Match, Expr} | Rest], Current, Acc) ->
    [NewMatch] = correct_line_numbers([Match], Current, []),
    [NewExpr] = correct_line_numbers([Expr], Current, []),
    correct_line_numbers(Rest, Current, 
                         [{match, Current + Offset, NewMatch, NewExpr} | Acc]);
correct_line_numbers([{var, {added, Offset}, Name} | Rest], Current, Acc) ->
    correct_line_numbers(Rest, Current, [{var, Current + Offset, Name} | Acc]);
correct_line_numbers([{call, {added, Offset}, Mod, Args} | Rest], Current, Acc) ->
    [NewMod] = correct_line_numbers([Mod], Current, []),
    NewArgs = correct_line_numbers(Args, Current, []),
    correct_line_numbers(Rest, Current, [{call, Current + Offset, NewMod, NewArgs} | Acc]);
correct_line_numbers([{remote, {added, Offset}, Mod, Func} | Rest], Current, Acc) ->
    [NewMod] = correct_line_numbers([Mod], Current, []),
    [NewFunc] = correct_line_numbers([Func], Current, []),
    correct_line_numbers(Rest, Current, [{remote, Current + Offset, NewMod, NewFunc} | Acc]);
correct_line_numbers([{atom, {added, Offset}, Name} | Rest], Current, Acc) ->
    correct_line_numbers(Rest, Current, [{atom, Current + Offset, Name} | Acc]);
correct_line_numbers([{tuple, {added, Offset}, Content} | Rest], Current, Acc) ->
    NewContent = correct_line_numbers(Content, Current, []),
    correct_line_numbers(Rest, Current, [{tuple, Current + Offset, NewContent} | Acc]);
correct_line_numbers([{'case', {added, Offset}, Expr, Clauses} | Rest], Current, Acc) ->
    [NewExpr] = correct_line_numbers([Expr], Current, []),
    NewClauses = correct_line_numbers(Clauses, Current, []),
    correct_line_numbers(Rest, Current, [{'case', Current + Offset, NewExpr, NewClauses} | Acc]);
correct_line_numbers([{cons, {added, Offset}, Head, Tail} | Rest], Current, Acc) ->
    [NewHead] = correct_line_numbers([Head], Current, []),
    [NewTail] = correct_line_numbers([Tail], Current, []),
    correct_line_numbers(Rest, Current, [{cons, Current + Offset, NewHead, NewTail} | Acc]);
correct_line_numbers([{nil, {added, Offset}} | Rest], Current, Acc) ->
    correct_line_numbers(Rest, Current, [{nil, Current + Offset} | Acc]);
correct_line_numbers([{integer, {added, Offset}, Val} | Rest], Current, Acc) ->
    correct_line_numbers(Rest, Current, [{integer, Current + Offset, Val} | Acc]);
correct_line_numbers([{float, {added, Offset}, Val} | Rest], Current, Acc) ->
    correct_line_numbers(Rest, Current, [{float, Current + Offset, Val} | Acc]);
correct_line_numbers([Else | Rest], _, Acc) ->
    correct_line_numbers(Rest, element(2, Else), [Else | Acc]);
correct_line_numbers([], _, Acc) ->
    lists:reverse(Acc).
