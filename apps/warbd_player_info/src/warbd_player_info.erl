
-module(warbd_player_info).
-vsn("1.0.0").

-behaviour(table_service).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").
-include_lib("erlangx/include/table_service.hrl").


-export([start_link/0, child_spec/2, tables/0, table_info/1]).
-export([faction/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    {
    }).

    
-record(db_player_info, 
    { player_id             :: warbd_type:player_id()
    , name                  :: string()
    , faction               :: warbd_type:faction()
    , last_update
    }).
    
    
-record(count_stats_type,
    { value                 :: non_neg_integer()
    , monthly               :: non_neg_integer()
    , weekly                :: non_neg_integer()
    , daily                 :: non_neg_integer()
    , one_life_max          :: non_neg_integer()
    }).
    
    
-record(db_player_stats,
    { player_id             :: warbd_type:player_id()
    , last_update
    , last_login
    , battle_rank           :: non_neg_integer()
    , login_count           :: pos_integer()
    , minutes_played        :: non_neg_integer()
    , score                 :: #count_stats_type{}
    , kills                 :: #count_stats_type{}
    , deaths                :: #count_stats_type{}
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
                    []          -> gen_server:call(?SERVER, {update_player_info, PlayerId})
                ;   [PlayerRec] -> PlayerRec
                end,
    (Player#db_player_info.faction).
        

%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting player_info"),
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%


%http://census.daybreakgames.com/s:warboard/get/ps2:v2/character/?character_id=5428010917272893697&c:show=name,faction_id
handle_call({update_player_info, PlayerId}, _From, #state{} = State) ->
    Player = #db_player_info{ player_id = PlayerId, name = "bob", faction = faction_nc },
    mnesia:activity(transaction,
            fun() ->
                mnesia:write(Player)
            end ),
    {reply, Player, State};
    
    
% http://census.daybreakgames.com/s:warboard/get/ps2:v2/character/?character_id=5428010917272893697&c:resolve=stat&c:resolve=online_status
%handle_call({update_player_stats, PlayerId}, _From, #state{} = State) ->
    
handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
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


