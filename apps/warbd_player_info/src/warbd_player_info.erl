
-module(warbd_player_info).
-vsn("1.0.0").

-behaviour(table_service).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").
-include_lib("erlangx/include/table_service.hrl").
-include_lib("warboard/include/warboard_records.hrl").


-export([start_link/0, child_spec/2, tables/0, table_info/1]).
-export([faction/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    {
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
        

%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting player_info"),
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call({fetch_player_info, PlayerId}, _From, #state{} = State) ->
    Player = warbd_query:get_player_info(PlayerId),
    mnesia:activity(transaction,
            fun() ->
                mnesia:write(Player)
            end ),
    {reply, Player, State};
    
    
%handle_call({fetch_player_stats, PlayerId}, _From, #state{} = State) ->
    
handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_cast({update_player_info, PlayerId}, #state{} = State) ->
    {noreply, State};
    
    
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


