-module(bliki.app.atom.atom_sup).
-behavior(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-import(supervisor).

start_link(Args) ->
    supervisor:start_link(bliki.app.atom.atom_sup, Args).

init(_Args) ->
    {ok, {{one_for_one, 3, 10},
          [{bliki.app.atom.atom, {bliki.app.atom.atom, start_link, []},
            permanent, 10, worker, [bliki.app.atom.atom]}]}}.
