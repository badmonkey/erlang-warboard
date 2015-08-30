
-module(warbd_statistics_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(warbd_statistics),
    lager:info("Started warbd_statistics server"),
    application:load(warbd_statistics).
    
    
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
    

