
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
    { player_id				:: warbd_type:player_id()
    , name					:: string()
    , faction				:: warbd_type:faction()
    , last_update
    }).
    
    
-record(count_stats_type,
	{ value					:: pos_integer()
	, monthly				:: pos_integer()
	, weekly				:: pos_integer()
	, daily					:: pos_integer()
	, one_life_max			:: pos_integer()
	}).
	
    
-record(db_player_stats,
	{ player_id				:: warbd_type:player_id()
	, last_login
	, battle_rank			:: integer()
	, score					:: #count_stats_type{}
	, kills					:: #count_stats_type{}
	, deaths				:: #count_stats_type{}
	}).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Public API


-spec faction( warbd_type:player_id() ) -> warbd_type:faction().

faction(_PlayerId) ->
    faction_tr.
    
    
%%%%% ------------------------------------------------------- %%%%%


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


tables() ->
    [db_player_info, db_player_stats].
    

table_info(Table) ->
    [ ?FIELDS(Table)
    , ?TABLEDB
    ].
    

%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting player_info"),
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    lager:info("warbd_player_info:call stopped ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:info("warbd_player_info:cast stopped ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    lager:info("warbd_player_info:info stopped ~p", [_Info]),
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


