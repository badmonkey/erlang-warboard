
-module(warbd_player_info).
-vsn("1.0.0").

-behaviour(table_service).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").
-include_lib("erlangx/include/table_service.hrl").
-include_lib("warboard/include/warboard_records.hrl").


-export([start_link/0, child_spec/2, tables/0, table_info/1]).
-export([faction/1, world/1, world_faction/1, name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { pending_player_info       :: #{ warbd_type:player_id() => [type:server_from()] }
    , pending_player_stats      :: #{ warbd_type:player_id() => [type:server_from()] }
    }).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    table_service:start_link(?MODULE, ?SERVER, []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


tables() ->
    [db_player_info, db_player_stats].
    

table_info(Table) ->
    [ ?FIELDS(Table)
    , ?TABLEDB
    ].

    
%%%%% ------------------------------------------------------- %%%%%


-spec faction( warbd_type:player_id() ) -> warbd_type:faction().

faction(PlayerId) ->
    Player =    case mnesia:dirty_read(db_player_info, PlayerId) of
                    []          -> gen_server:call(?SERVER, {fetch_player_info, PlayerId}, infinity)
                ;   [PlayerRec] -> PlayerRec
                end,
    (Player#db_player_info.faction).
    
    
-spec world( warbd_type:player_id() ) -> warbd_type:world().

world(PlayerId) ->
    Player =    case mnesia:dirty_read(db_player_info, PlayerId) of
                    []          -> gen_server:call(?SERVER, {fetch_player_info, PlayerId}, infinity)
                ;   [PlayerRec] -> PlayerRec
                end,
    (Player#db_player_info.world).    
        
        
-spec world_faction( warbd_type:player_id() ) -> {warbd_type:world(), warbd_type:faction()}.

world_faction(PlayerId) ->
    Player =    case mnesia:dirty_read(db_player_info, PlayerId) of
                    []          -> gen_server:call(?SERVER, {fetch_player_info, PlayerId}, infinity)
                ;   [PlayerRec] -> PlayerRec
                end,
    {Player#db_player_info.world, Player#db_player_info.faction}.
    
    
-spec name( warbd_type:player_id() ) -> string().

name(PlayerId) ->
    Player =    case mnesia:dirty_read(db_player_info, PlayerId) of
                    []          -> gen_server:call(?SERVER, {fetch_player_info, PlayerId}, infinity)
                ;   [PlayerRec] -> PlayerRec
                end,
    (Player#db_player_info.name).
    

%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting player_info"),
    EvtChannel = warbd_channel:new(),
    
    publisher:subscribe(EvtChannel, warbd_channel:player_info()),
    
    % TODO monitor warbd_query otherwise we'll get stuck waiting for replies
    
    { ok
    , #state{ pending_player_info = #{}
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call( {fetch_player_info, PlayerId}, From
           , #state{ pending_player_info = InfoMap } = State) ->
           
    case maps:get(PlayerId, InfoMap, undefined) of
        undefined   ->
            warbd_query:request_player_info(PlayerId),
            {noreply, State#state{ pending_player_info = InfoMap#{ PlayerId => [From] } } }
            
    ;   FromList    ->
            {noreply, State#state{ pending_player_info = InfoMap#{ PlayerId := [From | FromList] } } }
    end;
    
    
%handle_call({fetch_player_stats, PlayerId}, _From, #state{} = State) ->
    
handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%
    
    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_info( {pubsub_post, #db_player_info{ player_id = PlayerId } = NewPlayer}
           , #state{ pending_player_info = InfoMap } = State ) ->
    mnesia:activity(transaction,
                    fun() ->
                        mnesia:write(NewPlayer)
                    end ),
                    
    case maps:get(PlayerId, InfoMap, undefined) of
        undefined   ->
            lager:warning("No one waiting for Player ~p, skipping", [PlayerId]),
            {noreply, State}
            
    ;   FromList    ->
            [ gen_server:reply(X, NewPlayer) || X <- FromList ],
            {noreply, State#state{ pending_player_info = maps:remove(PlayerId, InfoMap) } }
    end;

    
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


