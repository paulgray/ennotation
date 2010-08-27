%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc parse_transform module for user-defined annotations.
%%%
%%% The idea to achieve this is to transform the functions using the
%%% following algorithm:
%%%
%%% If we have a function called mymod:foobar and want to surround it with
%%% some 'before' and 'after' calls:
%%% -BEFORE_LOG(SomeArgs).
%%% foobar(Foo, Bar) ->
%%%    "foobar!".
%%% 
%%% we will transform it into:
%%% foobar(Foo, Bar) ->
%%%    case before_log(SomeArgs, ?MODULE, foobar, [Foo, Bar]) of
%%%         {ok, Args} ->
%%%             apply_local('UNIQUE_TAG'_foobar, [Args]);
%%%         {stop, Result} ->
%%%             Result
%%%    end.
%%%
%%% 'UNIQUE_TAG'_foobar(Foo, Bar) ->
%%%    "foobar!".
%%%
%%% Note that apply_local/2 would be a standard call to foobar, with
%%% checks regarding the length of Args list (must be equal to the
%%% length of the initial arguments list.
%%%
%%% Having multiple 'before' annotations would follow the same algorithm
%%% recursively. If annotation callback function returns {ok, NewArgs}
%%% the next in line 'before' annotation will be called. In turn, if
%%% {stop, Result} is returned, next annotation callbacks will not be
%%% called, nor the actual function and 'after' annotation callbacks.
%%%
%%% The other case is having 'after' annotations:
%%% -AFTER_LOG(SomeArgs).
%%% foobar(Foo, Bar) ->
%%%    "foobar!".
%%%
%%% we will transform it into:
%%% foobar(Foo, Bar) ->
%%%    Result = apply_local('UNIQUE_TAG'_foobar, [Foo, Bar]),
%%%    after_log(SomeArgs, ?MODULE, foobar, Result).
%%%
%%% The chain of 'after' callbacks calls can never be broken, thus all 
%%% functions will be invoked. 
%%%
%%% @end
%%% Created : 26 Aug 2010 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ennotation_transform).

-export([parse_transform/2]).

-spec(parse_transform/2 :: (list(), list()) -> list()).
parse_transform(Tree, _Options) ->
    transform_tree(Tree, [], [], []).

-spec(transform_tree/4 :: (list(), list(), list(), list()) -> list()).
transform_tree([{attribute, _, module, Name} = A | Rest], Tree, [], []) ->
    put(module_name, Name),
    transform_tree(Rest, [A | Tree], [], []);
transform_tree([{attribute, _, user_ennotation, {Args, before, Mod, Func}} | Rest], 
               Tree, Before, After) ->
    transform_tree(Rest, Tree, [{Args, Mod, Func} | Before], After);
transform_tree([{attribute, _, user_ennotation, {Args, 'after', Mod, Func}} | Rest], 
               Tree, Before, After) ->
    transform_tree(Rest, Tree, Before, [{Args, Mod, Func} | After]);
transform_tree([F | Rest], Tree, [], []) ->
    transform_tree(Rest, [F | Tree], [], []);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, Before, After) ->
    NewF = transform_function(F, Before, After),
    transform_tree(Rest, [NewF | Tree], [], []);
transform_tree([Element | Rest], Tree, Before, After) ->
    transform_tree(Rest, [Element | Tree], Before, After);
transform_tree([], Tree, _, _) ->
    lists:reverse(Tree).

%% Before and After lists are coming in reverse order, but 
%% as we built it from the end it is correct
-spec(transform_function/3 :: (tuple(), list(), list()) -> tuple()).
transform_function(Func, Before, After) ->
    NewFunc = transform_function_before(Func, Before),
    transform_function_after(NewFunc, After).

%% FIXME: mocked function
-spec(transform_function_before/2 :: (tuple(), list()) -> tuple()).
transform_function_before({function, Line, Name, Artiy, Clauses} = F, 
                          [{Args, AMod, AFunc} | Rest]) ->
    F.

-spec(transform_function_after/2 :: (tuple(), list()) -> tuple()).
transform_function_after({function, Line, Name, Artity, Clauses} = F,
                         [{Args, AMod, AFunc} | Rest]) ->
    OrgFuncName = unique_function_name(Name, AFunc),
    ClauseArgs = generate_args(Artity),
    NewClause = {clause, Line, ClauseArgs, [], NewBody},

    {function, Line, Name, Artity, [NewClause]}.

-spec(prepare_arguments_list/2 :: (list(), integer()) -> tuple()).
prepare_arguments_list([H | R], Line) ->
    {cons, Line, H,
     prepare_arguments_list(R, Line)};
prepare_arguments_list([], Line) ->
    {nil, Line}.

-spec(get_unique_atom/0 :: () -> atom()).	     
get_unique_atom() ->
    list_to_atom(lists:flatten(io_lib:format("~w", [now()]))).

-spec(unique_function_name/2 :: (atom(), atom()) -> atom()).
unique_function_name(OrgFun, AFun) ->
    list_to_atom(lists:flatten(io_lib:format("~w_~w_~w", [erlang:phash2({OrgFun, AFun}), 
                                                          OrgFun, AFun]))).

-spec(prepare_annotations/2 :: (list(), integer()) -> term()).
prepare_annotations(Annotations, Line) ->
    NewLines = [$\n || _ <- lists:seq(1, Line-1)],
    SAnnotations = NewLines ++ lists:flatten(io_lib:format("~w.", [Annotations])),
    {ok, Tokens, _} = erl_scan:string(SAnnotations),
    {ok, [Parsed]} = erl_parse:parse_exprs(Tokens),

    Parsed.

%% FIXME - add line number
-spec(generate_args/1 :: (integer()) -> list()).    
generate_args(Arity) ->
    lists:reverse(lists:foldl(fun(N, Acc) ->
                                      [{var, 1, list_to_atom("Arg" ++ N)} | Acc]
                              end, [], lists:seq(1, Arity))).
