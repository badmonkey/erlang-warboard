
-module(warboard_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/0, start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(warboard),
    lager:info("Started Warboard server"),
    application:load(warboard).
    
    
start(_StartType, _StartArgs) ->
    warboard_sup:start_link().


stop(_State) ->
    ok.
    
