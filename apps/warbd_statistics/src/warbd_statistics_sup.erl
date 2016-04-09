
-module(warbd_statistics_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API

    
start_link() ->
    ?START_SUPERVISOR( warbd_statistics_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(warbd_statistics_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ warbd_statistics
            ] )      
      }
    }.
    
