%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc Parse transform callback module, responsible for
%%%      morphing modules that include "ennotation.hrl" header file.
%%%
%%%      There are 
%%%
%%%      
%%%
%%% @end
%%% Created : 25 Aug 2010 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ennotation).

-export([parse_transform/2]).

-record(state, {
          annotations :: list(), %% list of all annotations for the given module
          module_name :: atom(), %% module name
          module_path :: string() %% path to the module sources
         }).

-spec(parse_transform/2 :: (list(), list()) -> list()).
parse_transform(Tree, _Options) ->
    transform_tree(Tree, [], none, #state{}).

-spec(transform_tree/4 :: (list(), list(), none | before | 'after', #state{}) -> list()).
transform_tree([{attribute, _, ennotation_before, _} | Rest], Tree, _, State) ->
    transform_tree(Rest, Tree, before, State);
transform_tree([{attribute, _, ennotation_after, _} | Rest], Tree, _, State) ->
    transform_tree(Rest, Tree, 'after', State);
transform_tree([{attribute, _, module, Name} = A | Rest], Tree, AnnotationType, State) ->
    transform_tree(Rest, [A | Tree], AnnotationType, State#state{module_name = Name});
transform_tree([{attribute, _, file, {Path, _}} = A | Rest], Tree, _, State) ->
    transform_tree(Rest, [A | Tree], none, State#state{module_path = Path});
transform_tree([{function, _, _, _, _} = F | Rest], Tree, none, State) ->
    transform_tree(Rest, [F | Tree], none, State);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, AnnotationType, State) ->
    NewState = transform_function(F, AnnotationType, State),
    transform_tree(Rest, [F | Tree], none, NewState);
transform_tree([Element | Rest], Tree, AnnotationType, State) ->
    transform_tree(Rest, [Element | Tree], AnnotationType, State);
transform_tree([], Tree, _, State) ->
    HrlName = filename:join([filename:dirname(State#state.module_path), 
                             "..", "include", 
                             atom_to_list(State#state.module_name) ++ 
                                 "_annotations.hrl"]),
    ok = filelib:ensure_dir(HrlName),
    case file:open(HrlName, [write]) of
	{ok, Fd} ->
            io:format(Fd, "-compile({parse_transform, ennotation_transform}).~n"
                      "-compile(nowarn_shadow_vars).~n~n", []),
            save_annotations(Fd, lists:reverse(State#state.annotations));
        {error, Reason} ->
            error_logger:error_msg("Error during annotation header creation: ~p. "
                                   "Reason: ~p~n",
                                   [HrlName, Reason])
    end,
    lists:reverse(Tree).

-spec(transform_function/3 :: (tuple(), before | 'after', #state{}) -> #state{}).
transform_function({function, _, FunName, 4, _}, Type, State) ->
    State#state{annotations = [{Type, State#state.module_name, FunName} | 
                               State#state.annotations]};
transform_function({function, LineNo, FunName, _, _}, Type, State) ->
    error_logger:warning_msg("~p.erl:~p: function ~p must be of arity 4, skipping ~p annotation~n", 
                             [State#state.module_name, LineNo, FunName, Type]),
    State.

-spec(save_annotations/2 :: (pid(), list()) -> ok).
save_annotations(Hrl, [{Type, ModName, FunName} | Rest]) ->
    io:format(Hrl, "-define(~s(Args), -user_ennotation({Args, ~p, ~p, ~p})).~n~n",
	      [generate_define_name(FunName), Type, ModName, FunName]),
    save_annotations(Hrl, Rest);
save_annotations(Hrl, []) ->
    file:close(Hrl).

-spec(generate_define_name/1 :: (atom()) -> (string())).	     
generate_define_name(FunName) ->
    string:to_upper(atom_to_list(FunName)).
