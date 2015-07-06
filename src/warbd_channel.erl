
-module(warbd_channel).


-export([new/0, player_event/0, player_event/1]).


%%%%% ------------------------------------------------------- %%%%%


new() ->
    publisher:new(census_events).
    
    
player_event() ->
    player_event(match_one).
    
player_event(Faction) ->
    [player, Faction, event].    

