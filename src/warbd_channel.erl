
-module(warbd_channel).


-export([ new/0
        , player_event/0, player_event/1, player_event/2
        , player_info/0, player_info/1, player_info/2
        , player_status/0, player_status/1, player_status/2]).


%%%%% ------------------------------------------------------- %%%%%


new() ->
    publisher:new(census_events).

    
%%%%% ------------------------------------------------------- %%%%%
    
    
player_event() ->
    player_event(match_one, match_one).
    
player_event(World) ->
    player_event(World, match_one).
    
player_event(World, Faction) ->
    [player, World, Faction, event].    

    
%%%%% ------------------------------------------------------- %%%%%


player_info() ->
    player_info(match_one, match_one).
    
player_info(World) ->
    player_info(World, match_one).
    
player_info(World, Faction) ->
    [player, World, Faction, info].
        
        
%%%%% ------------------------------------------------------- %%%%%


player_status() ->
    player_status(match_one, match_one).
    
player_status(World) ->
    player_status(World, match_one).
    
player_status(World, Faction) ->
    [player, World, Faction, status].
