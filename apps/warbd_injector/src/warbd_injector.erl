
-module(warbd_injector).
-vsn("1.0.0").

-behaviour(websock_client).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2, websock_info/0, handle_frame/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

         
         
-define(SHORT_PERIOD, 2*60*1000).
-define(CHECK_PERIOD, 15*60*1000).
-define(LONG_PERIOD,  1*60*60*1000).

                     
%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { evtchannel
    , last_status_update    :: warbd_type:timestamp()
    
    , short_timer_ref       :: reference()
    , last_short_update     :: warbd_type:timestamp()
    
    , check_timer_ref       :: reference()
    , last_check_update     :: warbd_type:timestamp()
    
    , long_timer_ref        :: reference()
    , last_long_update      :: warbd_type:timestamp()
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
    Now = xtime:unix_time(),
    { ok
    , #state{ evtchannel = warbd_channel:new()
            , last_status_update = Now
            
            , short_timer_ref = erlang:start_timer(?SHORT_PERIOD, self(), interval)
            , last_short_update = Now
            
            , check_timer_ref = erlang:start_timer(?CHECK_PERIOD, self(), interval)
            , last_check_update = Now
            
            , long_timer_ref = erlang:start_timer(?LONG_PERIOD, self(), interval)
            , last_long_update = Now
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


handle_frame({system, up}, #state{} = State) ->
    { ok
    , [ #{ <<"service">> => <<"event">>
         , <<"action">> => <<"subscribe">>
         , <<"worlds">> => [<<"25">>]
         , <<"eventNames">> => [ <<"PlayerLogin">>, <<"PlayerLogout">>
                               , <<"FacilityControl">>, <<"MetagameEvent">>]
         }
      %, #{ <<"service">> => <<"event">>
      %   , <<"action">> => <<"subscribe">>
      %   , <<"characters">> => [<<"all">>]
      %   , <<"worlds">> => [<<"25">>]
      %   , <<"eventNames">> => [ <<"GainExperience">>
      %                         , <<"Death">>, <<"VehicleDestroy">>
      %                         , <<"PlayerFacilityDefend">>, <<"PlayerFacilityCapture">>]
      %   }
      ]
    , State};

    
handle_frame({json, #{ <<"send this for help">> := _Msg}}, #state{} = State) ->
    lager:debug("frame HELP"),
    {ok, State}; 
    
    
handle_frame({json, #{ <<"type">> := <<"heartbeat">> } = Data }, #state{} = State) ->
    handle_heartbeat( maps:get(<<"online">>, Data), State );
    
    
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


handle_heartbeat( #{} = Data
                , #state{ last_status_update = PrevTime } = State ) ->
    Now = xtime:unix_time(),
    Interval = Now - PrevTime,
    
    maps:fold(
          fun(K, V, ok) ->
            World = warboard_info:world_tag(K),

            publisher:notify( State#state.evtchannel
                            , warbd_channel:server_status(World)
                            , {heartbeat, World, jsonx:as_boolean(V), Now, Interval}),
            ok
          end
        , ok
        , Data),
    {ok, State#state{ last_status_update = Now }}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, #state{} = State) ->
    lager:warning("call UNKNOWN ~p", [_Request]),
    {reply, ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, #state{} = State) ->
    lager:warning("cast UNKNOWN ~p", [_Msg]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_info( {timeout, TimerRef, interval}
           , #state{ short_timer_ref = TimerRef
                   , last_short_update = PrevTime } = State) ->
    erlang:cancel_timer(TimerRef),
    Now = xtime:unix_time(),
    
    publisher:notify( State#state.evtchannel
                    , warbd_channel:server_event()
                    , {short_period_timer, Now, Now - PrevTime}),

    { noreply
    , State#state{ last_short_update = Now
                 , short_timer_ref = erlang:start_timer(?SHORT_PERIOD, self(), interval)
                 }
    };
    
    
handle_info( {timeout, TimerRef, interval}
           , #state{ check_timer_ref = TimerRef
                   , last_check_update = PrevTime } = State) ->
    erlang:cancel_timer(TimerRef),
    Now = xtime:unix_time(),
    
    publisher:notify( State#state.evtchannel
                    , warbd_channel:server_event()
                    , {check_period_timer, Now, Now - PrevTime}),

    { noreply
    , State#state{ last_check_update = Now
                 , check_timer_ref = erlang:start_timer(?CHECK_PERIOD, self(), interval)
                 }
    };
    
    
handle_info( {timeout, TimerRef, interval}
           , #state{ long_timer_ref = TimerRef
                   , last_long_update = PrevTime } = State) ->
    erlang:cancel_timer(TimerRef),
    Now = xtime:unix_time(),
    
    publisher:notify( State#state.evtchannel
                    , warbd_channel:server_event()
                    , {long_period_timer, Now, Now - PrevTime}),
                    
    { noreply
    , State#state{ last_long_update = Now
                 , long_timer_ref = erlang:start_timer(?LONG_PERIOD, self(), interval)
                 }
    };    

    
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


