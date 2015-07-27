
-module(warbd_presence).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).


-include_lib("erlangx/include/supervisors.hrl").


-export([oldest_player/1, population/1]).
-export([start_link/1, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { evtchannel
    , presence_table
    , login_table
    , world                 :: warbd_type:world()
    , first_event           :: warbd_type:timestamp() | undefined
    , oldest_player         :: {warbd_type:player_id(), warbd_type:timestamp()} | undefined
    , count                 :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link(World) ->
    gen_server_base:start_link_name(server_name(World), ?MODULE, [World]).

    
child_spec(Id, Args) -> ?SERVICE_SPEC(Id, ?MODULE, Args).


%%%%% ------------------------------------------------------- %%%%%


%info(Player) -> {online, World, OnSince} | offline


-spec population( warbd_type:world() ) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

population(World) ->
    gen_server:call( server_name(World), {population} ).
    

-spec oldest_player( warbd_type:world() ) -> {warbd_type:player_id(), warbd_type:timestamp()} | undefined.

oldest_player(World) ->
    gen_server:call( server_name(World), {oldest_player} ).

    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init([World]) ->
    lager:notice("Starting presence service for ~p", [World]),
    EvtChannel = warbd_channel:new(),
    
    PresenceTable = ets:new( table_name(World, presence), [protected, set, named_table, {read_concurrency, true}, {keypos, 1}]),
    LoginTable = ets:new( table_name(World, login), [protected, ordered_set, named_table, {read_concurrency, true}, {keypos, 1}]),
    
    publisher:subscribe(EvtChannel, warbd_channel:player_event(World)),
    
    { ok
    , #state{ evtchannel = EvtChannel
            , presence_table = PresenceTable
            , login_table = LoginTable
            , world = World
            , first_event = undefined
            , oldest_player = undefined
            , count = {0, 0, 0}
            }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call( {oldest_player}
           , _From
           , #state{ oldest_player = Oldest } = State) ->
    {reply, Oldest, State};
    
    
handle_call( {population}
           , _From
           , #state{ count = Count } = State) ->
    {reply, Count, State};    
    

handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_info( {login, PlayerId, World, Faction, Timestamp}
           , #state{ world = World
                   , count = Count
                   , presence_table = PTab
                   , login_table = LTab } = State) ->
    lager:notice("presence LOGIN ~p ~p ~p", [PlayerId, World, Faction]),
    
    Presence = {PlayerId, Timestamp},
    
    ets:insert(PTab, Presence),
    ets:insert(LTab, {{Timestamp, PlayerId}}),
    
    
    NState =    case {State#state.first_event, State#state.oldest_player} of
                    {undefined, undefined}  -> State#state{ first_event = Timestamp, oldest_player = Presence }
                ;   {undefined, _}          -> State#state{ first_event = Timestamp }
                ;   {_, undefined}          -> State#state{ oldest_player = Presence }
                ;   _                       -> State
                end,
                 
    { noreply
    , NState#state{ count = update_count(1, Faction, Count)
                  , first_event = NewFirst
                  }
    };
    
    
handle_info( {logout, PlayerId, World, Faction, _Timestamp}
           , #state{ world = World
                   , oldest_player = Oldest
                   , count = Count
                   , presence_table = PTab
                   , login_table = LTab } = State) ->
    lager:notice("presence LOGOUT ~p ~p ~p", [PlayerId, World, Faction]),
    
    case ets:lookup(PTab, PlayerId) of
        []          -> ok
    ;   [{P, L}]    ->
            ets:delete(LTab, {L,P}),
            ets:delete(PTab, P)
    end,
    
    NState =    case Oldest of
                    {PlayerId, _}   ->
                        case ets:first(LTab) of
                            {NwL, NwP}      -> State#state{ oldest_player = {NwP,NwL} }
                        ;   '$end_of_table' -> State#state{ oldest_player = undefined }
                        end
                        
                ;   _               ->
                        State
                end,
                
    MState =    case State#state.first_event of
                    undefined   -> NState#state{ first_event = Timestamp }
                ;   _           -> NState
                end,
                
    { noreply
    , MState#state{ count = update_count(-1, Faction, Count)
                  }
    };

    
handle_info(_Info, State) ->
    lager:warning("info UNKNOWN ~p", [_Info]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


update_count(N, faction_vs, {VS, NC, TR}) -> {max(0, VS+N), NC,             TR};
update_count(N, faction_nc, {VS, NC, TR}) -> {VS,           max(0, NC+N),   TR};
update_count(N, faction_tr, {VS, NC, TR}) -> {VS,           NC,             max(0, TR+N)}.



server_name(briggs) -> warbd_presence_briggs;
server_name(jaeger) -> warbd_presence_jaeger.


table_name(briggs,presence) -> briggs_presence_table;
table_name(briggs,login)    -> briggs_login_table;
table_name(jaeger,presence) -> jaeger_presence_table;
table_name(jaeger,login)    -> jaeger_login_table.

