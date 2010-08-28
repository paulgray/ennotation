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
    io:format("Original tree: ~p~n~n~n", [Tree]),
    NewTree = transform_tree(Tree, [], [], []),
    io:format("Modified tree: ~p~n", [NewTree]),
    NewTree.

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
    {NewF, AddedFuncs} = transform_function(F, Before, After),
    transform_tree(Rest, lists:append([NewF | AddedFuncs], Tree), [], []);
transform_tree([Element | Rest], Tree, Before, After) ->
    transform_tree(Rest, [Element | Tree], Before, After);
transform_tree([], Tree, _, _) ->
    lists:reverse(Tree).

%% Before and After lists are coming in reverse order, but 
%% as we built it from the end it is correct
-spec(transform_function/3 :: (tuple(), list(), list()) -> {tuple(), list()}).
transform_function(Func, Before, After) ->
    {NewFunc, AddedFuncs} = transform_function_before(Func, Before, []),
    transform_function_after(NewFunc, After, AddedFuncs).

%% FIXME: mocked function
-spec(transform_function_before/3 :: (tuple(), list(), list()) -> {tuple(), list()}).
transform_function_before({function, Line, Name, Arity, Clauses}, 
                          [{Args, AMod, AFunc} | Rest], Added) ->
    OrgFuncName = unique_function_name(Name, AFunc),
    ClauseArgs = generate_args(Arity, Line),

    NewBody = [{call, Line, 
                {atom, Line, Name},
                ClauseArgs}],
    NewClause = {clause, Line, ClauseArgs, [], NewBody},
    OrgFunc = {function, Line, OrgFuncName, Arity, Clauses},

    transform_function_before({function, Line, Name, Arity, [NewClause]}, 
                              Rest, [OrgFunc | Added]);
transform_function_before(F, [], Added) ->
    {F, Added}.

-spec(transform_function_after/3 :: (tuple(), list(), list()) -> {tuple(), list()}).
transform_function_after({function, Line, Name, Arity, Clauses},
                         [{Args, AMod, AFunc} | Rest], Added) ->
    OrgFuncName = unique_function_name(Name, AFunc),
    ClauseArgs = generate_args(Arity, Line),
    ResultVar = unique_atom(),
    AnnotationArgs = prepare_annotation_args(Args, Line),
    
    NewBody = [{match, Line, 
                {var, Line, ResultVar},
                {call, Line, 
                 {atom, Line, OrgFuncName}, 
                 ClauseArgs}},
               {call, Line, 
                {remote, Line, 
                 {atom, Line, AMod}, 
                 {atom, Line, AFunc}},
                [AnnotationArgs,
                 {atom, Line, get(module_name)},
                 {atom, Line, Name},
                 {var, Line, ResultVar}
                ]}],
    NewClause = {clause, Line, ClauseArgs, [], NewBody},
    OrgFunc = {function, Line, OrgFuncName, Arity, Clauses},

    transform_function_after({function, Line, Name, Arity, [NewClause]},
                             Rest, [OrgFunc | Added]);
transform_function_after(F, [], Added) ->
    {F, Added}.

-spec(prepare_arguments_list/2 :: (list(), integer()) -> tuple()).
prepare_arguments_list([H | R], Line) ->
    {cons, Line, H,
     prepare_arguments_list(R, Line)};
prepare_arguments_list([], Line) ->
    {nil, Line}.

-spec(unique_atom/0 :: () -> atom()).	     
unique_atom() ->
    list_to_atom(lists:flatten(io_lib:format("~w", [now()]))).

-spec(unique_function_name/2 :: (atom(), atom()) -> atom()).
unique_function_name(OrgFun, AFun) ->
    list_to_atom(lists:flatten(io_lib:format("~w_~w_~w", [erlang:phash2({now(), OrgFun, AFun}), 
                                                          OrgFun, AFun]))).

-spec(prepare_annotation_args/2 :: (list(), integer()) -> term()).
prepare_annotation_args(Annotations, Line) ->
    NewLines = [$\n || _ <- lists:seq(1, Line-1)],
    SAnnotations = NewLines ++ lists:flatten(io_lib:format("~w.", [Annotations])),
    {ok, Tokens, _} = erl_scan:string(SAnnotations),
    {ok, [Parsed]} = erl_parse:parse_exprs(Tokens),

    Parsed.

-spec(generate_args/2 :: (integer(), integer()) -> list()).    
generate_args(Arity, Line) ->
    lists:reverse(lists:foldl(fun(N, Acc) ->
                                      [{var, Line, 
                                        list_to_atom("Arg" ++ integer_to_list(N))} | Acc]
                              end, [], lists:seq(1, Arity))).
