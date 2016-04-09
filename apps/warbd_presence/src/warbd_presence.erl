
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


-type pop_count() :: { Value :: type:natural(), Min :: type:natural(), Max :: type:natural() }.

    
-record(interval_stats,
    { total                 :: pop_count()
    , event_count           :: { type:natural(), type:natural(), type:natural() }
    , faction               :: { pop_count(), pop_count(), pop_count() }
    }).


-record(state,
    { evtchannel
    , presence_table
    , login_table
    , world                 :: warbd_type:world()
    , first_event           :: warbd_type:timestamp()
    , last_update           :: warbd_type:timestamp()
    , oldest_player         :: { warbd_type:player_id(), warbd_type:timestamp() } | undefined
    , stats                 :: #interval_stats{}
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

    publisher:subscribe(EvtChannel, warbd_channel:server_event()),
    publisher:subscribe(EvtChannel, warbd_channel:player_event(World)),

    
    { ok
    , #state{ evtchannel = EvtChannel
            , presence_table = PresenceTable
            , login_table = LoginTable
            , world = World
            , first_event = xtime:unix_time()
            , last_update = xtime:unix_time()
            , oldest_player = undefined
            , stats = #interval_stats{ total = new_pop_count(0)
                                     , event_count = new_pop_count(0)
                                     , faction = { new_pop_count(0)
                                                 , new_pop_count(0)
                                                 , new_pop_count(0)
                                                 }
                                     }
            }
    }.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call( {oldest_player}
           , _From
           , #state{ oldest_player = Oldest } = State) ->
    {reply, Oldest, State};
    
    
handle_call( {population}
           , _From
           , #state{ stats = Data } = State) ->
    {reply, get_faction_counts(Data), State};    
    

handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

  
handle_info( {pubsub_post, {short_period_timer, _, _}}
           , #state{ world = World
                   , stats = #interval_stats{ total = TotalPop
                                            , event_count = {EvtIn, EvtOut, EvtDel}
                                            , faction = {VS, NC, TR}
                                            } = Stats
                   , first_event = First
                   , last_update = PrevTime
                   , oldest_player = Oldest } = State) ->
    
    OldestInfo =    case Oldest of
                        undefined               -> "none"
                    ;   {OldId, OldestLogin}    ->
                            xstring:format("~s ~s ago", [warbd_player_info:name(OldId), text:timesince(OldestLogin)])
                    end,
    lager:notice( "Presence(~p): uptime ~s, oldest ~s"
                , [ World, text:timesince(First), OldestInfo ]),
                   
    Now = xtime:unix_time(),
    Interval = Now - PrevTime,

    publisher:notify( State#state.evtchannel
                    , warbd_channel:population_event(World)
                    , { pop_change
                      , World, Now, Interval
                      , {EvtIn, EvtOut, EvtOut - EvtDel}
                      , TotalPop
                      , #{ faction_vs => VS, faction_nc => NC, faction_tr => TR }
                      }),
 
    {noreply, State#state{ stats = reset_stats(Stats), last_update = Now } };
    
    
handle_info( {pubsub_post, {check_period_timer, _, _}}
           , #state{ oldest_player = Oldest } = State) ->
           
    case Oldest of
        undefined       -> ok
    ;   {PlayerId, _}   ->
            lager:debug("Checking online status ~p", [PlayerId]),
            warbd_query:request_player_status(PlayerId)
    end,
    
    {noreply, State};
    
    
handle_info( {pubsub_post, {login, PlayerId, World, Faction, Timestamp}}
           , #state{ world = World
                   , stats = Stats
                   , presence_table = PTab
                   , login_table = LTab } = State) ->
    lager:info("presence LOGIN ~p ~p ~p", [PlayerId, World, Faction]),
    
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
    , NState#state{ stats = update_stats(login, Faction, Stats) }
    };
    
    
handle_info( {pubsub_post, {logout, PlayerId, World, Faction, Timestamp}}
           , #state{ world = World
                   , oldest_player = Oldest
                   , stats = Stats
                   , presence_table = PTab
                   , login_table = LTab } = State) ->
    lager:info("presence LOGOUT ~p ~p ~p", [PlayerId, World, Faction]),
    
    Action =    case ets:lookup(PTab, PlayerId) of
                    []          -> ghost
                ;   [{P, L}]    ->
                        ets:delete(LTab, {L,P}),
                        ets:delete(PTab, P),

                        publisher:notify( State#state.evtchannel
                                        , warbd_channel:player_status(World, Faction)
                                        , {playtime, PlayerId, World, Faction, Timestamp - L, erlang:element(1, Oldest) =:= PlayerId}),

                        logout
                end,
    
    NState =    case Oldest of
                    {PlayerId, _}   ->
                        case ets:first(LTab) of
                            {NwL, NwP}      ->
                                warbd_query:request_player_status(NwP),
                                
                                publisher:notify( State#state.evtchannel
                                                , warbd_channel:player_status(World, Faction)
                                                , {oldest, PlayerId, World, Faction, Timestamp - NwL}),
                                        
                                State#state{ oldest_player = {NwP,NwL} }

                        ;   '$end_of_table' ->
                                State#state{ oldest_player = undefined }
                        end
                        
                ;   _               ->
                        State
                end,
                
    MState =    case State#state.first_event of
                    undefined   -> NState#state{ first_event = Timestamp }
                ;   _           -> NState
                end,
                
    { noreply
    , MState#state{ stats = update_stats(Action, Faction, Stats) }
    };

    
handle_info( {pubsub_post, {online, PlayerId, World, Faction, Online}}
           , #state{ world = World
                   , presence_table = PTab } = State) ->
                   
    InTable =   case ets:lookup(PTab, PlayerId) of
                    []          -> false
                ;   [{_, _}]    -> true
                end,
                
    lager:info("online status for ~p.. in table: ~p, online: ~p ", [PlayerId, InTable, Online]),
                
    case {InTable, Online} of
        {X, X}          -> ok   % table agrees with API
        
    ;   {true, false}   ->      % send a fake event to evict person from table.
                                % If this may cause a duplicate logout event.
                                % The second event will be handled as a ghost logout.
            publisher:notify( State#state.evtchannel
                            , warbd_channel:player_event(World, Faction)
                            , {logout, PlayerId, World, Faction, xtime:unix_time()})

    ;   {false, true}   ->      % Don't send a fake event, we could be in a race with a
                                % real login event making our count wrong
                                % @todo handle 'duplicate' login events
            %publisher:notify( State#state.evtchannel
            %                , warbd_channel:player_event(World, Faction)
            %                , {login, PlayerId, World, Faction, xtime:unix_time()})
            ok
    end,
    
    {noreply, State};
    
    
handle_info( {pubsub_post, _}
           , #state{} = State) ->    
    {noreply, State};
    
    
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


server_name(briggs) -> warbd_presence_briggs;
server_name(jaeger) -> warbd_presence_jaeger.


table_name(briggs,presence) -> briggs_presence_table;
table_name(briggs,login)    -> briggs_login_table;
table_name(jaeger,presence) -> jaeger_presence_table;
table_name(jaeger,login)    -> jaeger_login_table.


%%%%% ------------------------------------------------------- %%%%%


-spec adjust( login | ghost | logout, type:natural() ) -> type:natural().

adjust(login, X)    -> X + 1;
adjust(ghost, X)    -> X;    
adjust(logout, 0)   -> 0;   
adjust(logout, X)   -> X - 1.


%%%%% ------------------------------------------------------- %%%%%


faction_get(faction_vs, {VS, _, _}) -> VS;    
faction_get(faction_nc, {_, NC, _}) -> NC;
faction_get(faction_tr, {_, _, TR}) -> TR.

faction_set(faction_vs, X, {_, NC, TR}) -> {X,  NC, TR};
faction_set(faction_nc, X, {VS, _, TR}) -> {VS, X,  TR};
faction_set(faction_tr, X, {VS, NC, _}) -> {VS, NC, X }.

    
%%%%% ------------------------------------------------------- %%%%%


new_pop_count(X) -> { X, X, X }.
    

%%%%% ------------------------------------------------------- %%%%%


update_evt_count(login, {Logins, Logouts, Deleted}) -> {Logins + 1, Logouts, Deleted};
update_evt_count(ghost, {Logins, Logouts, Deleted}) -> {Logins, Logouts + 1, Deleted};
update_evt_count(logout, {Logins, Logouts, Deleted}) -> {Logins, Logouts + 1, Deleted + 1}.


%%%%% ------------------------------------------------------- %%%%%


-spec update_count( type:natural(), pop_count() ) -> pop_count().

update_count(Value, {_, Min, Max}) ->
    if
        Value < Min -> {Value, Value, Max}
    ;   Value > Max -> {Value, Min, Value}
    ;   true        -> {Value, Min, Max}
    end.
    

%%%%% ------------------------------------------------------- %%%%%


get_faction_counts( #interval_stats{ faction = {VS, NC, TR} } ) ->
    { get_count(VS)
    , get_count(NC)
    , get_count(TR)
    }.
    
get_count( {Count, _, _} ) -> Count.


%%%%% ------------------------------------------------------- %%%%%


-spec update_stats( login | ghost | logout, warbd_type:faction(), #interval_stats{} ) -> #interval_stats{}.

update_stats( What, Faction
            , #interval_stats{ total = {Total, _, _} = TotalPop
                             , event_count = EvtCount
                             , faction = Faction_stats }) ->
    NewTotal = adjust(What, Total),
    NewFaction = update_faction_stats(What, faction_get(Faction, Faction_stats)),

    #interval_stats{ total = update_count(NewTotal, TotalPop)
                   , event_count = update_evt_count(What, EvtCount)
                   , faction = faction_set(Faction, NewFaction, Faction_stats)
                   }.
                   
   
%%%%% ------------------------------------------------------- %%%%%


-spec update_faction_stats( login | ghost | logout, pop_count() ) -> pop_count().
                       
update_faction_stats( What
                    , { Count, _, _ } = PopCount ) ->                
    NewCount = adjust(What, Count),
    update_count(NewCount, PopCount).

                  
%%%%% ------------------------------------------------------- %%%%%


reset_stats( #interval_stats{ total = {Total, _, _}
                            , faction = {VS, NC, TR} } ) ->               
    #interval_stats{ total = new_pop_count(Total)
                   , event_count = new_pop_count(0)
                   , faction = { new_pop_count( get_count(VS) )
                               , new_pop_count( get_count(NC) )
                               , new_pop_count( get_count(TR) )
                               }
                   }.
                   
                   
                   
                   