
-module(warbd_channel).


-export([new/0, player_event/0, player_event/2]).


%%%%% ------------------------------------------------------- %%%%%


new() ->
    publisher:new(census_events).
    
    
player_event() ->
    player_event(match_one, match_one).
    
player_event(World, Faction) ->
    [player, World, Faction, event].    

