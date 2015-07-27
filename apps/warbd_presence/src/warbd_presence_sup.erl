
-module(warbd_presence_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( warbd_presence_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(warbd_presence_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ {warbd_presence_briggs, warbd_presence, [briggs]}
            , {warbd_presence_jaeger, warbd_presence, [jaeger]}
            ] )      
      }
    }.
    

