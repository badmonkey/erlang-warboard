
-module(warbd_channel).


-export([new/0]).


%%%%% ------------------------------------------------------- %%%%%


new() ->
    publisher:new(census_events).

