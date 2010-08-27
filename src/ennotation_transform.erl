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
    NewF = transform_function(F, lists:reverse(Before), lists:reverse(After)),
    transform_tree(Rest, [NewF | Tree], [], []);
transform_tree([Element | Rest], Tree, Before, After) ->
    transform_tree(Rest, [Element | Tree], Before, After);
transform_tree([], Tree, _, _) ->
    lists:reverse(Tree).

-spec(transform_function/3 :: (tuple(), list(), list()) -> tuple()).
transform_function({function, Line, FunName, Arity, Clauses}, Before, After) ->
    put(function_name, FunName),
    put(function_arity, Arity),

    NewClauses = transform_clause(Clauses, Before, After, []),
    {function, Line, FunName, Arity, NewClauses}.

-spec(transform_clause/4 :: (list(), list(), list(), list()) -> list()).
transform_clause([OrgClause | Rest], Before, After, Clauses) ->
    BeforeClause = transform_clause_before(OrgClause, Before),
    AfterClause = transform_clause_after(BeforeClause, After),
    transform_clause(Rest, Before, After, [AfterClause | Clauses]);
transform_clause([], _, _, Clauses) ->
    lists:reverse(Clauses).

-spec(transform_clause_before/2 :: (tuple(), list()) -> tuple()).
transform_clause_before(Clause, []) ->
    Clause;
transform_clause_before({clause, L, CArgs, Guards, Body}, Annotations0) ->
    TgtFunM = get(module_name),
    TgtFunN = get(function_name),
    Arity = get(function_arity),

    Annotations = prepare_annotations(Annotations0, L),

    AFunName = get_unique_atom(),
    FuncArgs = get_unique_atom(),
    AArgs = get_unique_atom(),
    Mod = get_unique_atom(),
    Func = get_unique_atom(),
    Rest = get_unique_atom(),
    Self = get_unique_atom(),
    NewArgs = get_unique_atom(),
    EMod = get_unique_atom(),
    EFunc = get_unique_atom(),

    NewBody = [{match, L,
                {var, L, AFunName},
                {'fun', L,
                 {clauses,
                  [{clause, L,
                    [{var, L, FuncArgs},
                     {cons, L,
                      {tuple, L,
                       [{var, L, AArgs}, {var, L, Mod}, {var, L, Func}]},
                      {var, L, Rest}},
                     {var, L, Self}],
                    [],
                    [{'case', L,
                      {call, L,
                       {remote, L, {var, L, Mod}, {var, L, Func}},
                       [{var, L, AArgs},
                        {atom, L, TgtFunM},
                        {atom, L, TgtFunN},
                        {var, L, FuncArgs}]},
                      [{clause, L,
                        [{tuple, L, [{atom, L, proceed}, {var, L, NewArgs}]}],
                        [],
                        [{call, L,
                          {var, L, Self},
                          [{var, L, NewArgs}, {var, L, Rest}, {var, L, Self}]}]},
                       {clause, L, 
                        [{tuple, L, [{atom, L, skip}, {var, L, NewArgs}]}],
                        [],
                        [{var, L, NewArgs}]},
                       {clause, L,
                        [{tuple, L,
                          [{atom, L, error},
                           {tuple, L,
                            [{var, L, EMod},
                             {var, L, EFunc},
                             {var, L, NewArgs}]}]}],
                        [],
                        [{call, L, 
                          {atom, L, apply},
                          [{var, L, EMod},
                           {var, L, EFunc},
                           {var, L, NewArgs}]}]}]}]},
                   {clause, L, 
                    [{var, L, FuncArgs}, {nil, L}, {var, L, '_'}],
                    [],
                    [{'if', L,
                      [{clause, L, [],
                        [[{op, L, '=/=',
                           {call, L, {atom, L, length}, [{var, L, FuncArgs}]},
                           {integer, L, Arity}}]],
                        [{call, L,
                          {atom, L, throw},
                          [{tuple, L,
                            [{atom, L, bad_annotation_result_length},
                             {call, L,
                              {atom, L, length},
                              [{var, L, FuncArgs}]}]}]}]},
                       {clause, L, [],
                        [[{atom, L, true}]],
                        [{call, L,
                          {atom, L, apply},
                          [{'fun', L,
                            {clauses, 
                             [{clause, L,
                               CArgs,
                               [],
                               Body}]}},
                           {var, L, FuncArgs}]}]}]}]}]}}},
               {call, L, 
                {var, L, AFunName},
                [prepare_arguments_list(CArgs, L),
                 Annotations,
                 {var, L, AFunName}]}],

    {clause, L, CArgs, Guards, NewBody}.

transform_clause_after(Clause, []) ->
    Clause;
transform_clause_after({clause, L, CArgs, Guards, Body}, Annotations0) ->
    TgtFunM = get(module_name),
    TgtFunN = get(function_name),

    Annotations = prepare_annotations(Annotations0, L),

    OrgFunName = get_unique_atom(),
    AFunName = get_unique_atom(),
    FuncResult = get_unique_atom(),
    AArgs = get_unique_atom(),
    Mod = get_unique_atom(),
    Func = get_unique_atom(),
    Rest = get_unique_atom(),
    Self = get_unique_atom(),
    NewResult = get_unique_atom(),
    EMod = get_unique_atom(),
    EFunc = get_unique_atom(),

    NewBody = [{match, L,
                {var, L, OrgFunName},
                {'fun', L, 
                 {clauses,
                  [{clause, L, [], [],
                    Body}]}}},
               {match, L, 
                {var, L, AFunName},
                {'fun', L,
                 {clauses,
                  [{clause, L,
                    [{var, L, FuncResult},
                     {cons, L,
                      {tuple, L,
                       [{var, L, AArgs}, {var, L, Mod}, {var, L, Func}]},
                      {var, L, Rest}},
                     {var, L, Self}],
                    [],
                    [{'case', L,
                      {call, L,
                       {remote, L, {var, L, Mod}, {var, L, Func}},
                       [{var, L, AArgs},
                        {atom, L, TgtFunM},
                        {atom, L, TgtFunN},
                        {var, L, FuncResult}]},
                      [{clause, L,
                        [{tuple, L, [{atom, L, proceed}, {var, L, NewResult}]}],
                        [],
                        [{call, L,
                          {var, L, Self},
                          [{var, L, NewResult}, {var, L, Rest}, {var, L, Self}]}]},
                       {clause, L, 
                        [{tuple, L, [{atom, L, skip}, {var, L, NewResult}]}],
                        [],
                        [{var, L, NewResult}]},
                       {clause, L,
                        [{tuple, L,
                          [{atom, L, error},
                           {tuple, L,
                            [{var, L, EMod},
                             {var, L, EFunc},
                             {var, L, NewResult}]}]}],
                        [],
                        [{call, L, 
                          {atom, L, apply},
                          [{var, L, EMod},
                           {var, L, EFunc},
                           {var, L, NewResult}]}]}]}]},
                   {clause, L, 
                    [{var, L, FuncResult}, {nil, L}, {var, L, '_'}],
                    [],
                    [{var, L, FuncResult}]}]}}},
               {call, L,
                {var, L, AFunName},
                [{call, L,
                  {var, L, OrgFunName}, []},
                 Annotations,
                 {var, L, AFunName}]}],

    {clause, L, CArgs, Guards, NewBody}.

-spec(prepare_arguments_list/2 :: (list(tuple()), integer()) -> tuple()).	     
prepare_arguments_list([H | R], Line) ->
    {cons, Line, H,
     prepare_arguments_list(R, Line)};
prepare_arguments_list([], Line) ->
    {nil, Line}.

-spec(get_unique_atom/0 :: () -> atom()).	     
get_unique_atom() ->
    list_to_atom(lists:flatten(io_lib:format("~w", [now()]))).

-spec(prepare_annotations/2 :: (list(), integer()) -> term()).
prepare_annotations(Annotations, Line) ->
    NewLines = [$\n || _ <- lists:seq(1, Line-1)],
    SAnnotations = NewLines ++ lists:flatten(io_lib:format("~w.", [Annotations])),
    {ok, Tokens, _} = erl_scan:string(SAnnotations),
    {ok, [Parsed]} = erl_parse:parse_exprs(Tokens),

    Parsed.
