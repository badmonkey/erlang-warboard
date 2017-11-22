
-module(war_specific_source).
-vsn("1.0.0").

-behaviour(websock_client).
-behaviour(supervisor_child).

-include_lib("erlangx/include/constants.hrl").
-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/1, child_spec/2, websock_info/0, handle_frame/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, server_name/1]).


                     
%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { evtchannel
    , last_status_update    :: warbd_type:timestamp()
    , world                 :: war_type:world()
    
    , short_timer_ref       :: reference()
    , last_short_update     :: warbd_type:timestamp()
    
    , check_timer_ref       :: reference()
    , last_check_update     :: warbd_type:timestamp()
    
    , long_timer_ref        :: reference()
    , last_long_update      :: warbd_type:timestamp()
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link(Opts) ->
    World = property:get_value(world, Opts),
    Result = websock_client:start_link(?MODULE, undefined, property:update(defer_startup, true, Opts)),
    startphase:register_started_service(server_name(World), Result).

    
child_spec(Id, Args) -> ?SERVICE_SPEC(Id, ?MODULE, [Args]).


websock_info(_Args) ->
    [ {url, xstring:format("wss://push.planetside2.com/streaming?environment=ps2&service-id=~s", [war_constant:census_id()])}
    ].
    
    
server_name(briggs) -> war_source_briggs;
server_name(jaeger) -> war_source_jaeger.
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Url, Opts) ->
    lager:notice("Starting census event ingest service"),
    Now = xtime:unix_time(),
    World = property:get_value(world, Opts),
    startphase:required([war_presence:server_name(World)]),
    
    startphase:defer_startup(
        { ok
        , #state{ evtchannel = war_channel:new()
                , world = World
                }
        }).

    
%%%%% ------------------------------------------------------- %%%%%


handle_frame({system, up}, #state{} = State) ->
    { ok
    , [ #{ <<"service">> => <<"event">>
         , <<"action">> => <<"subscribe">>
         , <<"worlds">> => [ war_constant:world_json(State#state.world) ]
         , <<"eventNames">> => [ <<"PlayerLogin">>, <<"PlayerLogout">>
%                               , <<"PlayerFacilityCapture">>, <<"PlayerFacilityDefend">>
%                               , <<"GainExperience">>
                               ]
         }
      ]
    , State};

    
handle_frame({json, #{ <<"send this for help">> := _Msg}}, #state{} = State) ->
    lager:debug("frame HELP"),
    {ok, State}; 
    
    
handle_frame({json, #{ <<"type">> := <<"heartbeat">> } = Data }, #state{} = State) ->
    lager:debug("frame HEARTBEAT"),
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
    

handle_event( #{ <<"event_name">> := <<"PlayerLogin">> } = Data
            , #state{ world = World } = State) ->
    lager:debug("event LOGIN ~p", [Data]),
    #{ <<"character_id">> := BinPlayerId
     , <<"timestamp">> := BinTimestamp
     } = Data,
     
    PlayerId = xerlang:binary_to_integer(BinPlayerId),
    Timestamp = war_constant:timestamp(BinTimestamp),
    
    publisher:notify( State#state.evtchannel
                    , war_channel:player_event(World)
                    , {login_raw, PlayerId, World, Timestamp}),
    {ok, State};

    
handle_event( #{ <<"event_name">> := <<"PlayerLogout">> } = Data
            , #state{ world = World } = State) ->
    lager:debug("event LOGOUT ~p", [Data]),
    #{ <<"character_id">> := BinPlayerId
     , <<"timestamp">> := BinTimestamp
     } = Data,
     
    PlayerId = xerlang:binary_to_integer(BinPlayerId),
    Timestamp = war_constant:timestamp(BinTimestamp),
    
    publisher:notify( State#state.evtchannel
                    , war_channel:player_event(World)
                    , {logout_raw, PlayerId, World, Timestamp}),
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


handle_info(startup, #state{} = State) ->
    {connect, State};
    
    
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

