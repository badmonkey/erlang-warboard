
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


%%%%% ------------------------------------------------------- %%%%%
% Server State


-type request_type() :: player_info | player_stats.
-type request() :: #{ warbd_type:player_id() => [type:server_from()] }.


-record(state,
    { census_id                         :: string()
    , pending       = init_pending()    :: #{ request_type() => request() }
    , request_type  = undefined         :: undefined | request_type()
    , httpc_ref
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


%%%%% ------------------------------------------------------- %%%%%


-spec get_player_info( warbd_type:player_id() ) -> #db_player_info{}.

get_player_info(PlayerId) ->
    gen_server:call(?SERVER, {get_player_info, PlayerId}).
    
    
get_player_stats(PlayerId) ->
    ok.


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting api query here"),
    {ok, #state{ census_id = warboard_info:census_id() }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call( {get_player_info, PlayerId}, From
           , #state{ request_type = undefined
                   , pending = #{ player_info := Requests } = Pending } = State) ->
    lager:info("Query request for player_info ~p", [PlayerId]),
    NewState = State#state{
                    pending = Pending#{ player_info := add_request(Requests, PlayerId, From) }
                },
    {noreply, start_request(player_info, NewState)};
    
    
handle_call( {get_player_info, PlayerId}, From
           , #state{ pending = #{ player_info := Requests } = Pending } = State) ->
    NewState = State#state{
                    pending = Pending#{ player_info := add_request(Requests, PlayerId, From) }
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

add_request( #{} = Requests, PlayerId, From) ->
    case maps:is_key(PlayerId, Requests) of
        true    ->
            Fromlist = maps:get(PlayerId, Requests),
            maps:put(PlayerId, [From | Fromlist], Requests)
            
    ;   false   ->
            maps:put(PlayerId, [From], Requests)
    end.


%%%%% ------------------------------------------------------- %%%%%


start_pending_requests(#state{ pending = Pending } = State) ->
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


start_request(player_info, #state{ census_id = Census, pending = #{ player_info := Requests } } = State) ->
    Players = maps_take(?MAX_PLAYER_REQUEST, Requests), 
    case Players of
        []  -> State
        
    ;   _   ->
        Ids = string:join([integer_to_list(X) || X <- Players], ","),
        Url = xstring:format( "http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:show=character_id,name,faction_id"
                            , [Census, Ids] ),
        
        {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
        
        State#state{
                  request_type = player_info
                , httpc_ref = RequestId
            }
    end;


% http://census.daybreakgames.com/~s/get/ps2:v2/character/?character_id=~s&c:resolve=stat&c:resolve=online_status
    
start_request(player_stats, #state{} = State) ->
    State.    
    
    
%%%%% ------------------------------------------------------- %%%%%


process_response(player_info, Result, State) ->
    State;
    
    
process_response(player_stats, Result, State) ->
    % generate player_info responses also
    State.
    
    
%%%%% ------------------------------------------------------- %%%%%


maps_take(N, Requests) ->
    {_, R} =    maps:fold(
                    fun (_, _, {0, R1}) -> {0, R1}
                    ;   (K, _, {X, R2}) -> {X-1, [K | R2]}
                    end,
                    {N, []},
                    Requests),
    R.
