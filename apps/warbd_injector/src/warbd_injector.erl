
-module(warbd_injector).
-vsn("1.0.0").

-behaviour(websock_client).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2, websock_info/0, handle_frame/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { evtchannel
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    websock_client:start_link(?SERVER, ?MODULE).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


websock_info() ->
    [ {url, xstring:format("wss://push.planetside2.com/streaming?environment=ps2&service-id=~s", [warboard_info:census_id()])}
    ].
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:notice("Starting census event ingest service"),
    {ok, #state{ evtchannel = warbd_channel:new() }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_frame({system, up}, #state{} = State) ->
    { ok
    , #{ <<"service">> => <<"event">>
       , <<"action">> => <<"subscribe">>
       , <<"worlds">> => [<<"25">>]
       , <<"eventNames">> => [<<"PlayerLogin">>, <<"PlayerLogout">>]
       }
    , State};

    
handle_frame({json, #{ <<"send this for help">> := _Msg}}, #state{} = State) ->
    lager:debug("frame HELP"),
    {ok, State}; 
    
    
handle_frame({json, #{ <<"type">> := <<"heartbeat">> } = Data }, #state{} = State) ->
    % send heartbeat message to pubsub system
    lager:debug("frame HEARBEAT ~p", [Data]),
    {ok, State};    
    
    
handle_frame({json, #{ <<"type">> := <<"serviceStateChanged">> } = Data }, #state{} = State) ->
    lager:debug("frame STATECHANGE service ~p", [Data]),
    {ok, State};
    

handle_frame({json, #{ <<"type">> := <<"connectionStateChanged">> } }, #state{} = State) ->
    lager:debug("frame STATECHANGE connection"),
    {ok, State};    
    
    
handle_frame({json, #{ <<"type">> := <<"serviceMessage">> } = Data }, #state{} = State) ->
    handle_event( maps:get(<<"payload">>, Data), State);    


handle_frame({json, #{ <<"subscription">> := Data } }, #state{} = State) ->
    lager:debug("frame SUBSCRIPTION ~p", [Data]),
    {ok, State};
    

handle_frame(Msg, #state{} = State) ->
    lager:warning("frame UNHANDLED ~p", [Msg]),
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
    

handle_event( #{ <<"event_name">> := <<"PlayerLogin">> } = Data, #state{} = State) ->
    lager:debug("event LOGIN ~p", [Data]),
    #{ <<"character_id">> := BinPlayerId
     , <<"timestamp">> := BinTimestamp
     } = Data,
     
    PlayerId = xerlang:binary_to_integer(BinPlayerId),
    {World, Faction} = warbd_player_info:world_faction(PlayerId),
    Timestamp = warboard_info:timestamp(BinTimestamp),
    
    publisher:notify( State#state.evtchannel
                    , warbd_channel:player_event(World, Faction)
                    , {login, PlayerId, World, Faction, Timestamp}),
    {ok, State};

    
handle_event( #{ <<"event_name">> := <<"PlayerLogout">> } = Data, #state{} = State) ->
    lager:debug("event LOGOUT ~p", [Data]),
    #{ <<"character_id">> := BinPlayerId
     , <<"timestamp">> := BinTimestamp
     } = Data,
     
    PlayerId = xerlang:binary_to_integer(BinPlayerId),
    {World, Faction} = warbd_player_info:world_faction(PlayerId),
    Timestamp = warboard_info:timestamp(BinTimestamp),
    
    publisher:notify( State#state.evtchannel
                    , warbd_channel:player_event(World, Faction)
                    , {logout, PlayerId, World, Faction, Timestamp}),
    {ok, State};
    
    
handle_event(Data, #state{} = State) ->
    lager:warning("event UNHANDLED ~p", [Data]),
    {ok, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, #state{} = State) ->
    lager:warning("call UNKNOWN ~p", [_Request]),
    {reply, ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, #state{} = State) ->
    lager:warning("cast UNKNOWN ~p", [_Msg]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, #state{} = State) ->
    lager:warning("info UNKNOWN ~p", [_Info]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


