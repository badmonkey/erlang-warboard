
-module(warbd_player_info_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(warbd_player_info),
    lager:info("Started warbd_player_info server"),
    application:load(warbd_player_info).
    
    
start_link() ->
    ?START_SUPERVISOR( warbd_player_info_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(warbd_player_info_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ warbd_player_info
            ] )      
      }
    }.
    

