
-module(war_global_source).
-vsn("1.0.0").

-behaviour(websock_client).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/constants.hrl").
-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/1, child_spec/2, websock_info/0, handle_frame/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, server_name/1]).

         
-define(SHORT_PERIOD, 2  * ?MILLISEC_PER_MIN).
-define(CHECK_PERIOD, 15 * ?MILLISEC_PER_MIN).
-define(LONG_PERIOD,  1  * ?MILLISEC_PER_HOUR).

                     
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


start_link(Opts) ->
    Result = websock_client:start_link(?MODULE, undefined, property:update(defer_startup, true, Opts)),
    startphase:register_started_service(?SERVER, Result).

    
child_spec(Id, Args) -> ?SERVICE_SPEC(Id, ?MODULE, [Args]).


websock_info(_Args) ->
    [ {url, xstring:format("wss://push.planetside2.com/streaming?environment=ps2&service-id=~s", [war_constant:census_id()])}
    ].

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Url, Opts) ->
    lager:notice("Starting census global event ingest service"),
    Now = xtime:unix_time(),
    startphase:required([war_player_info, war_statistics]),
    
    startphase:defer_startup(
        { ok
        , #state{ evtchannel = war_channel:new()
                , last_status_update = Now
                
                , short_timer_ref = erlang:start_timer(?SHORT_PERIOD, self(), interval)
                , last_short_update = Now
                
                , check_timer_ref = erlang:start_timer(?CHECK_PERIOD, self(), interval)
                , last_check_update = Now
                
                , long_timer_ref = erlang:start_timer(?LONG_PERIOD, self(), interval)
                , last_long_update = Now
                }
        }).

    
%%%%% ------------------------------------------------------- %%%%%


handle_frame({system, up}, #state{} = State) ->
    { ok
    , [ #{ <<"service">> => <<"event">>
         , <<"action">> => <<"subscribe">>
         , <<"worlds">> => [ <<"all">> ]
         , <<"eventNames">> => [ <<"FacilityControl">>, <<"MetagameEvent">>, <<"ContinentLock">>
%                               , <<"Death">>, <<"VehicleDestroy">>
                               ]
         }
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
            World = war_constant:world_tag(K),

            publisher:notify( State#state.evtchannel
                            , war_channel:server_status(World)
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
                    , war_channel:server_event()
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
                    , war_channel:server_event()
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
                    , war_channel:server_event()
                    , {long_period_timer, Now, Now - PrevTime}),
                    
    { noreply
    , State#state{ last_long_update = Now
                 , long_timer_ref = erlang:start_timer(?LONG_PERIOD, self(), interval)
                 }
    };    


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

