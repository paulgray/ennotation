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
%% TODO:
%% - documentation
%% - correct line numbers
%% - consider putting external 'specs' in Erlang consultable file
%%   (e.g. by integrating with meck)
-module(ennotation_transform).

-export([parse_transform/2]).

-spec(parse_transform/2 :: (list(), list()) -> list()).
parse_transform(Tree, _Options) ->
    NewTree = transform_tree(Tree, [], [], []),
    io:format("~p~n", [NewTree]),
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
    {NewF, AddedFuncs} = transform_function(F, Before, lists:reverse(After)),
    transform_tree(Rest, lists:append([NewF | AddedFuncs], Tree), [], []);
transform_tree([Element | Rest], Tree, Before, After) ->
    transform_tree(Rest, [Element | Tree], Before, After);
transform_tree([], Tree, _, _) ->
    ennotation_line:correct_line_numbers(lists:reverse(Tree), 1, []).

%% we need to transform the function using 'after' annotations in first order
%% as 'before' annotation might stop the function calls (and thus bypass 'after'
%% call flow)
-spec(transform_function/3 :: (tuple(), list(), list()) -> {tuple(), list()}).
transform_function(Func, Before, After) ->
    {NewFunc, AddedFuncs} = transform_function_after(Func, After, []),
    transform_function_before(NewFunc, Before, AddedFuncs).

-spec(transform_function_before/3 :: (tuple(), list(), list()) -> {tuple(), list()}).
transform_function_before({function, Line, Name, Arity, Clauses}, 
                          [{Args, AMod, AFunc} | Rest], Added) ->
    OrgFuncName = unique_function_name(Name, AFunc),
    ClauseArgs = generate_args(Arity, "Arg", added(Line, 0)),
    AnnotationArgs = prepare_annotation_args(Args, added(Line, 1)),
    IntClauseArgs = prepare_arguments_list(ClauseArgs, added(Line, 1)),
    ResVars = generate_args(Arity, "Res", added(Line, 3)),
    ResArgs = prepare_arguments_list(ResVars, added(Line, 2)),

    NewBody = [{'case', added(Line, 1), 
                {call, added(Line, 1), 
                 {remote, added(Line, 1),
                  {atom, added(Line, 1), AMod},
                  {atom, added(Line, 1), AFunc}},
                 [AnnotationArgs,
                  {atom, added(Line, 1), get(module_name)},
                  {atom, added(Line, 1), Name},
                  IntClauseArgs]},
                [{clause, added(Line, 2), 
                  [{tuple, added(Line, 2), 
                    [{atom, added(Line, 2), ok},
                     ResArgs]}],
                  [],
                  [{call, added(Line, 3), 
                    {atom, added(Line, 3), OrgFuncName},
                    ResVars}]},
                 {clause, added(Line, 4), 
                  [{tuple, added(Line, 4),
                    [{atom, added(Line, 4), stop},
                     {var, added(Line, 4), 'Stop'}]}],
                  [],
                  [{var, added(Line, 5), 'Stop'}]},
                 {clause, added(Line, 6), 
                  [{tuple, added(Line, 6),
                    [{atom, added(Line, 6), ok},
                     {var, added(Line, 6), 'Else'}]}],
                  [],
                  [{call, added(Line, 7),
                    {remote, added(Line, 7),
                     {atom, added(Line, 7), erlang},
                     {atom, added(Line, 7), throw}},
                    [{tuple, added(Line, 7),
                      [{atom, added(Line, 7), incompatible_before_annotation_result},
                       {var, added(Line, 7), 'Else'}]}]}]}]}],

    NewClause = {clause, added(Line, 0), ClauseArgs, [], NewBody},
    OrgFunc = {function, added(Line, 0), OrgFuncName, Arity, Clauses},

    transform_function_before({function, added(Line, 0), Name, Arity, [NewClause]}, 
                              Rest, [OrgFunc | Added]);
transform_function_before(F, [], Added) ->
    {F, Added}.

-spec(transform_function_after/3 :: (tuple(), list(), list()) -> {tuple(), list()}).
transform_function_after({function, Line, Name, Arity, Clauses},
                         [{Args, AMod, AFunc} | Rest], Added) ->
    OrgFuncName = unique_function_name(Name, AFunc),
    ClauseArgs = generate_args(Arity, "Arg", added(Line, 1)),
    AnnotationArgs = prepare_annotation_args(Args, added(Line, 2)),
    
    NewBody = [{match, added(Line, 1), 
                {var, added(Line, 1), 'Result'},
                {call, added(Line, 1), 
                 {atom, added(Line, 1), OrgFuncName}, 
                 ClauseArgs}},
               {call, added(Line, 2), 
                {remote, added(Line, 2), 
                 {atom, added(Line, 2), AMod}, 
                 {atom, added(Line, 2), AFunc}},
                [AnnotationArgs,
                 {atom, added(Line, 2), get(module_name)},
                 {atom, added(Line, 2), Name},
                 {var, added(Line, 2), 'Result'}
                ]}],
    NewClause = {clause, added(Line, 0), ClauseArgs, [], NewBody},
    OrgFunc = {function, added(Line, 0), OrgFuncName, Arity, Clauses},

    transform_function_after({function, added(Line, 0), Name, Arity, [NewClause]},
                             Rest, [OrgFunc | Added]);
transform_function_after(F, [], Added) ->
    {F, Added}.

-spec(prepare_arguments_list/2 :: (list(), integer()) -> tuple()).
prepare_arguments_list([H | R], Line) ->
    {cons, Line, H,
     prepare_arguments_list(R, Line)};
prepare_arguments_list([], Line) ->
    {nil, Line}.

-spec(unique_function_name/2 :: (atom(), atom()) -> atom()).
unique_function_name(OrgFun, AFun) ->
    list_to_atom(lists:flatten(io_lib:format("~w_~w_~w", [erlang:phash2({now(), OrgFun, AFun}), 
                                                          OrgFun, AFun]))).

-spec(prepare_annotation_args/2 :: (list(), integer()) -> term()).
prepare_annotation_args(Annotations, Line0) ->
    Line = line_no(Line0),
    NewLines = [$\n || _ <- lists:seq(1, Line-1)],
    SAnnotations = NewLines ++ lists:flatten(io_lib:format("~w.", [Annotations])),
    {ok, Tokens, _} = erl_scan:string(SAnnotations),
    {ok, [Parsed]} = erl_parse:parse_exprs(Tokens),

    Parsed.

-spec(generate_args/3 :: (integer(), string(), integer()) -> list()).    
generate_args(Arity, Prefix, Line) ->
    lists:reverse(lists:foldl(fun(N, Acc) ->
                                      [{var, Line, 
                                        list_to_atom(Prefix ++ integer_to_list(N))} | Acc]
                              end, [], lists:seq(1, Arity))).

-spec(added/2 :: (integer() | {added, integer()}, integer()) -> {added, integer()}).
added({added, Line}, Offset) ->
    {added, Line + Offset};
added(Line, Offset) ->
    {added, Line + Offset}.

-spec(line_no/1 :: (integer() | {added, integer()}) -> integer()).
line_no({added, Line}) ->
    Line;
line_no(Line) ->
    Line.
