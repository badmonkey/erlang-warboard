
-module(warbd_query).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").
-include_lib("warboard/include/warboard_records.hrl").


-export([start_link/0, child_spec/2]).
-export([get_player_info/1, get_player_stats/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(MAX_PLAYER_REQUEST, 10).
-define(REPORT_PERIOD, 2*60*1000).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-type request_type() :: player_info | player_stats.
-type request() :: #{ warbd_type:player_id() => [type:server_from()] }.


-record(state,
    { census_id                             :: string()
    , pending           = init_pending()    :: #{ request_type() => request() }
    , request_type      = undefined         :: undefined | request_type()
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


-spec get_player_info( warbd_type:player_id() ) -> #db_player_info{}.

get_player_info(PlayerId) ->
    gen_server:call(?SERVER, {get_player_info, PlayerId}, infinity).
    
    
get_player_stats(PlayerId) ->
    ok.


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:notice("Starting census api query service"),
    
    timer:send_interval(?REPORT_PERIOD, {period_interval}),
    
    { ok
    , #state{ census_id = warboard_info:census_id()
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


% No current request, queue it, start request
handle_call( {get_player_info, PlayerId}, From
           , #state{ request_type = undefined
                   , pending = #{ player_info := Requests } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ player_info := add_request(Requests, PlayerId, From) }
                },
    {noreply, start_request(player_info, NewState)};
    
    
% Request already in progress, just queue it    
handle_call( {get_player_info, PlayerId}, From
           , #state{ pending = #{ get_player_info := Requests } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ get_player_info := add_request(Requests, PlayerId, From) }
                },
    {noreply, NewState};



handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
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
    #{ player_info  => #{}
     , player_stats => #{} }.
    

%%%%% ------------------------------------------------------- %%%%%


-spec add_request( request(), warbd_type:player_id(), type:server_from() ) -> request().

add_request(#{} = Requests, PlayerId, From) ->
    case maps:is_key(PlayerId, Requests) of
        true    ->
            Fromlist = maps:get(PlayerId, Requests),
            maps:put(PlayerId, [From | Fromlist], Requests)
            
    ;   false   ->
            maps:put(PlayerId, [From], Requests)
    end.


%%%%% ------------------------------------------------------- %%%%%


% iterate through all pending request types trying to start a batch of requests
start_pending_requests( #state{ pending = Pending } = State) ->
    {_, NState} =   maps:fold(
                        fun (_, _, {true, S1}) -> {true, S1}
                        ;   (K, V, {false, S2}) ->
                                case maps:size(V) of
                                    0   -> {false, S2}
                                ;   _   -> {true, start_request(K, State)}
                                end
                        end,
                        {false, State#state { request_type = undefined }},
                        Pending),
    NState.

    
-define(PLAYER_INFO_QUERY, "http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:resolve=world&c:show=character_id,name,faction_id").
-define(PLAYER_STATS_QUERY, "http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:resolve=stat&c:resolve=online_status").

    

start_request( player_info
             , #state{ census_id = Census
                     , pending = #{ player_info := Requests }
                     , query_total = Total
                     , query_interval = Interval } = State) ->
    Players = xmaps:takekeys(?MAX_PLAYER_REQUEST, Requests), 
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
                , State) ->
    Json = jiffy:decode(Body, [return_maps]),

    lists:foldl(
        fun(CharJson, #state{ pending = #{ player_info := Requests } = Pending } = StateN) ->
            BinPlayerId = jsonx:get({"character_id"}, CharJson, jsthrow),
            PlayerId = xerlang:binary_to_integer(BinPlayerId),

            case maps:is_key(PlayerId, Requests) of
                true    ->
                    Fromlist = maps:get(PlayerId, Requests),
                    PlayerInfo = #db_player_info{
                              player_id = PlayerId
                            , name = binary_to_list( jsonx:get({"name", "first_lower"}, CharJson, jsthrow) )
                            , world = warboard_info:world( jsonx:get({"world_id"}, CharJson, jsthrow) )
                            , faction = warboard_info:faction( jsonx:get({"faction_id"}, CharJson, jsthrow) )
                            , last_update = xtime:unix_time()
                        },
                    lager:debug("RESPONSE replylist ~p ~p", [Fromlist, PlayerInfo]),
                    [ gen_server:reply(X, PlayerInfo) || X <- Fromlist ],
                    StateN#state{
                            pending = Pending#{ player_info := maps:remove(PlayerId, Requests) }
                        }
                    
            ;   false   ->
                    lager:warning("No one waiting for Player ~p, skipping", [PlayerId]),
                    StateN
            end

        end,
        State,
        jsonx:get({"character_list"}, Json, []) );
    
    
process_response(player_stats, Result, State) ->
    % generate player_info responses also
    State.
    
