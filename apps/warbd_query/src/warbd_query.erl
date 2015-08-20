
-module(warbd_query).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").
-include_lib("warboard/include/warboard_records.hrl").


-export([start_link/0, child_spec/2]).
-export([request_player_info/1, request_player_stats/1, request_player_status/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(MAX_PLAYER_REQUEST, 10).
-define(REPORT_PERIOD, 2*60*1000).
-define(PAUSE_TIME, 1*60*1000).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-type request_type() :: player_info | player_stats.
-type request() :: sets:set( warbd_type:player_id() ).


-record(state,
    { census_id                             :: string()
    , evtchannel
    , pending           = init_pending()    :: #{ request_type() => request() }
    , request_type      = undefined         :: undefined | pause | request_type()
    , httpc_ref
    , query_total       = 0                 :: non_neg_integer()
    , query_interval    = 0                 :: non_neg_integer()
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


%%%%% ------------------------------------------------------- %%%%%


-spec request_player_info( warbd_type:player_id() ) -> ok.

request_player_info(PlayerId) ->
    gen_server:cast(?SERVER, {fetch_player_info, PlayerId}),
    ok.
    
    
request_player_stats(PlayerId) ->
    ok.


-spec request_player_status( warbd_type:player_id() ) -> ok.
    
request_player_status(PlayerId) ->    
    gen_server:cast(?SERVER, {fetch_player_status, PlayerId}),
    ok.

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:notice("Starting census api query service"),
    EvtChannel = warbd_channel:new(),
    
    timer:send_interval(?REPORT_PERIOD, {period_interval}),
    
    { ok
    , #state{ census_id = warboard_info:census_id()
            , evtchannel = EvtChannel
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


% No current request, queue it, start request
handle_cast( {fetch_player_info, PlayerId}
           , #state{ request_type = undefined
                   , pending = #{ player_info := RequestSet } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ player_info := sets:add_element(PlayerId, RequestSet) }
                },
    {noreply, start_request(player_info, NewState)};
    
    
% Request already in progress, just queue it    
handle_cast( {fetch_player_info, PlayerId}
           , #state{ pending = #{ player_info := RequestSet } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ player_info := sets:add_element(PlayerId, RequestSet) }
                },
    {noreply, NewState};

    
    
handle_cast( {fetch_player_status, PlayerId}
           , #state{ request_type = undefined
                   , pending = #{ player_online := RequestSet } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ player_online := sets:add_element(PlayerId, RequestSet) }
                },
    {noreply, start_request(player_online, NewState)};

    
% Request already in progress, just queue it    
handle_cast( {fetch_player_status, PlayerId}
           , #state{ pending = #{ player_online := RequestSet } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ player_online := sets:add_element(PlayerId, RequestSet) }
                },
    {noreply, NewState};
    
    
    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_info( {resume}
           , #state{ request_type = pause } = State) ->
    {noreply, start_pending_requests(State)};
    

handle_info( {tcp_error, _Ref, etimedout}
           , #state{} = State) ->
    lager:error("Timeout with connection"),
    
    timer:send_after(?PAUSE_TIME, {resume}),
    
    {noreply, State#state{ request_type = pause, httpc_ref = undefined }};

    
handle_info( {http, {RequestId, {error, Reason}}}
           , #state{ httpc_ref = RequestId
                   , request_type = RType } = State) ->
    lager:error("Error with connection ~p ~p", [RType, Reason]),
    
    timer:send_after(?PAUSE_TIME, {resume}),
    
    {noreply, State#state{ request_type = pause, httpc_ref = undefined }};

    
handle_info( {http, {RequestId, Result}}
           , #state{ httpc_ref = RequestId
                   , request_type = RType } = State) ->
    NewState1 = process_response(RType, Result, State),
    NewState2 = start_pending_requests(NewState1),
    {noreply, NewState2};
    
    
handle_info( {period_interval}
           , #state{ query_total = Total
                   , query_interval = Interval } = State) ->
    lager:notice("QUERIES ~p, total ~p", [Interval, Total]),
    
    {noreply, State#state{ query_interval = 0 } };
    
    
handle_info(_Info, State) ->
    lager:error("info STOPPED ~p", [_Info]),
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


init_pending() ->
    #{ player_info  => sets:new()
     , player_stats => sets:new()
     , player_online => sets:new() }.
    

%%%%% ------------------------------------------------------- %%%%%


% iterate through all pending request types trying to start a batch of requests
start_pending_requests( #state{ pending = Pending } = State) ->
    {_, NState} =   maps:fold(
                        fun (_, _, {true, S1}) -> {true, S1}
                        ;   (K, V, {false, S2}) ->
                                case sets:size(V) of
                                    0   -> {false, S2}
                                ;   _   -> {true, start_request(K, State)}
                                end
                        end,
                        {false, State#state { request_type = undefined, httpc_ref = undefined }},
                        Pending),
    NState.

    
-define(PLAYER_INFO_QUERY, "http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:resolve=world&c:show=character_id,name,faction_id").
-define(PLAYER_STATS_QUERY, "http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:resolve=stat&c:resolve=online_status").
-define(ONLINE_STATUS_QUERY, "http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:resolve=world&c:resolve=online_status&c:show=character_id,faction_id,online_status").
    

start_request( player_info
             , #state{ census_id = Census
                     , pending = #{ player_info := RequestSet }
                     , query_total = Total
                     , query_interval = Interval } = State) ->
    Players = xsets:take(?MAX_PLAYER_REQUEST, RequestSet), 
    case Players of
        []  -> State
        
    ;   _   ->
            Ids = string:join([integer_to_list(X) || X <- Players], ","),
            Url = xstring:format(?PLAYER_INFO_QUERY, [Census, Ids]),
            
            lager:debug("player_info QUERY ~p", [Url]),
            {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
            
            State#state{ request_type = player_info
                       , httpc_ref = RequestId
                       , query_total = Total + 1
                       , query_interval = Interval + 1
                       }
    end;

    
start_request( player_online
             , #state{ census_id = Census
                     , pending = #{ player_online := RequestSet }
                     , query_total = Total
                     , query_interval = Interval } = State) ->
    Players = xsets:take(?MAX_PLAYER_REQUEST, RequestSet), 
    case Players of
        []  -> State
        
    ;   _   ->
            Ids = string:join([integer_to_list(X) || X <- Players], ","),
            Url = xstring:format(?ONLINE_STATUS_QUERY, [Census, Ids]),
            
            lager:debug("player_online QUERY ~p", [Url]),
            {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
            
            State#state{ request_type = player_online
                       , httpc_ref = RequestId
                       , query_total = Total + 1
                       , query_interval = Interval + 1
                       }
    end;
    
    
start_request(player_stats, #state{} = State) ->
    State.    
    
    
%%%%% ------------------------------------------------------- %%%%%


process_response( _Request
                , {error, socket_closed_remotely}
                , State) ->
    lager:warning("QUERY socket closed remotely"),
    State;
    

process_response( player_info
                , {{_Version, _Code, _ReasonPhrase}, _Headers, Body}
                , #state{} = State) ->
    Json = jiffy:decode(Body, [return_maps]),

    lists:foldl(
        fun(CharJson, #state{ pending = #{ player_info := RequestSet } = Pending } = StateN) ->
            try
                BinPlayerId = jsonx:get({"character_id"}, CharJson, jsthrow),
                PlayerId = xerlang:binary_to_integer(BinPlayerId),

                case sets:is_element(PlayerId, RequestSet) of
                    true    ->
                        World = warboard_info:world( jsonx:get({"world_id"}, CharJson, jsthrow) ),
                        Faction = warboard_info:faction( jsonx:get({"faction_id"}, CharJson, jsthrow) ),
                        
                        PlayerInfo = #db_player_info{
                                  player_id = PlayerId
                                , name = binary_to_list( jsonx:get({"name", "first_lower"}, CharJson, jsthrow) )
                                , world = World
                                , faction = Faction
                                , last_update = xtime:unix_time()
                            },

                        publisher:notify( State#state.evtchannel
                                        , warbd_channel:player_info(World, Faction)
                                        , PlayerInfo),
                        
                        StateN#state{
                                pending = Pending#{ player_info := sets:del_element(PlayerId, RequestSet) }
                            }
                        
                ;   false   ->
                        lager:warning("No one waiting for Player ~p, skipping", [PlayerId]),
                        StateN
                end
                
            catch
                throw:Error     ->
                    lager:error("Incorrectly structured json ~p ~p", [Error, CharJson]),
                    StateN
            end

        end,
        State,
        jsonx:get({"character_list"}, Json, []) );
    
    
process_response(player_stats, Result, State) ->
    % generate player_info responses also
    State;
    

process_response( player_online
                , {{_Version, _Code, _ReasonPhrase}, _Headers, Body}
                , #state{} = State) ->
    Json = jiffy:decode(Body, [return_maps]),

    lists:foldl(
        fun(CharJson, #state{ pending = #{ player_online := RequestSet } = Pending } = StateN) ->
            try
                BinPlayerId = jsonx:get({"character_id"}, CharJson, jsthrow),
                PlayerId = xerlang:binary_to_integer(BinPlayerId),

                case sets:is_element(PlayerId, RequestSet) of
                    true    ->
                        World = warboard_info:world( jsonx:get({"world_id"}, CharJson, jsthrow) ),
                        Faction = warboard_info:faction( jsonx:get({"faction_id"}, CharJson, jsthrow) ),
                        Online = jsonx:compare({"world_id"}, {"online_status"}, CharJson, jsthrow),
                        
                        lager:debug("RESPONSE ~p  online: ~p", [PlayerId, Online]),

                        publisher:notify( State#state.evtchannel
                                        , warbd_channel:player_status(World, Faction)
                                        , {online, PlayerId, World, Faction, Online}),
                        
                        StateN#state{
                                pending = Pending#{ player_online := sets:del_element(PlayerId, RequestSet) }
                            }
                        
                ;   false   ->
                        lager:warning("No one waiting for Player ~p, skipping", [PlayerId]),
                        StateN
                end
                
            catch
                throw:Error     ->
                    lager:error("Incorrectly structured json ~p ~p", [Error, CharJson]),
                    StateN
            end

        end,
        State,
        jsonx:get({"character_list"}, Json, []) ).
