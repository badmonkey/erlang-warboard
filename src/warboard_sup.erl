
-module(warboard_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( warboard_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(warboard_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ warboard
            , war_global_source
            , {source1, war_specific_source, #{ world => briggs }}
%            , {source2, war_specific_source, #{ world => jaeger }}
            , {presence1, war_presence, #{ world => briggs }}
%            , {presence2, war_presence, #{ world => jaeger }}
            , war_api_query
            , war_player_info
            , war_statistics
            ])
      }
    }.
    

