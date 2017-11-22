
-module(war_channel).


-export([ new/0, warbeard_pubsub/1
        , player_event/1, server_status/1, server_event/0
        
        , player_info/0, player_info/1, player_info/2
        , player_status/0, player_status/1, player_status/2
        , population_event/0, population_event/1]).


%%%%% ------------------------------------------------------- %%%%%


warbeard_pubsub(X)
    -> {pubsub_post, X}.
    

new() ->
    publisher:new(census_events, fun warbeard_pubsub/1).

    
%%%%% ------------------------------------------------------- %%%%%
    
    
player_event(any_world) ->
    [player, match_one, event];
    
player_event(World) ->
    [player, World, event]. 
    
    
%%%%% ------------------------------------------------------- %%%%%


server_status(any_world) ->
    [server, match_one, status];
    
server_status(World) ->
    [server, World, status].


%%%%% ------------------------------------------------------- %%%%%
    
    
server_event() ->
    [server, event].    

    
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

    
%%%%% ------------------------------------------------------- %%%%%


population_event() ->
    population_event(match_one).
    
population_event(World) ->
    [population, World, event].
    