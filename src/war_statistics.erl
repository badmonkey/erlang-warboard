
-module(warbd_statistics).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    {
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:notice("Starting statistics service"),
    EvtChannel = warbd_channel:new(),

    publisher:subscribe(EvtChannel, warbd_channel:population_event()),
    
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    lager:info("warbd_statistics:call stopped ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:info("warbd_statistics:cast stopped ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_info( { pubsub_post
             , { pop_change
               , World, _, _
               , {EvtIn, EvtOut, EvtGhosts}
               , {PopTotal, _, _} = Pop
               , #{} = FactionStats
               }
             }
           , #state{} = State ) ->
           
    lager:notice("Population(~p): ~s events: ~p in, ~p out, ~p ghosts"
                , [ World, format_range(Pop)
                  , EvtIn, EvtOut, EvtGhosts
                  ]),
                  
    lager:notice("Population(~p): ~s"
                , [ World
                  , format_faction(faction_vs, PopTotal, FactionStats)
                  ]),
    lager:notice("Population(~p): ~s"
                , [ World
                  , format_faction(faction_nc, PopTotal, FactionStats)
                  ]),
    lager:notice("Population(~p): ~s"
                , [ World
                  , format_faction(faction_tr, PopTotal, FactionStats)
                  ]),
                  
    {noreply, State};
    
    
handle_info(_Info, State) ->
    lager:info("warbd_statistics:info stopped ~p", [_Info]),
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


format_range({X, Min, Max}) ->
    xstring:format("~p (~p .. ~p)", [X, Min, Max]).

    
    
format_faction( Faction, 0, #{} ) ->
    xstring:format( "~s - 0.0%  0 (0 .. 0)", [ warboard_info:faction_name(Faction) ] );

    
format_faction( Faction
              , Total
              , #{} = FactionStats) ->
    format_faction(Faction, Total, maps:get(Faction, FactionStats));
    
              
format_faction( Faction
              , Total
              , {Count, _, _} = Values) ->              
    xstring:format( "~s - ~.1f%  ~s"
                  , [ warboard_info:faction_name(Faction)
                    , Count / Total * 100.0
                    , format_range(Values) ] ).    
