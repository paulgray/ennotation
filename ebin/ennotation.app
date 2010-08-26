%% -*- mode: erlang; -*-
{application, ennotation,
 [
  {description, "Erlang's Aspects implementation"},
  {vsn, "0.1"}, 
  {modules, [ennotation, ennotation_transform]},
  {registered, []},
  {applications, [kernel, stdlib, syntax_tools]},
  {build_dependencies, []},
  {env, []}
 ]}.
