
-module(warbd_channel).


-export([new/0, player_event/0]).


%%%%% ------------------------------------------------------- %%%%%


new() ->
    publisher:new(census_events).
    
    
player_event() ->
	[player, match_one, event].

