
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


-record(faction_stats,
    { count                 :: non_neg_integer()
    , minmax                :: {non_neg_integer(), non_neg_integer()}
    }).
    
-record(interval_stats,
    { total                 :: non_neg_integer()
    , minmax                :: {non_neg_integer(), non_neg_integer()}
    , event_count           :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
    , faction               :: { #faction_stats{}, #faction_stats{}, #faction_stats{} }
    }).


-record(state,
    { evtchannel
    , presence_table
    , login_table
    , world                 :: warbd_type:world()
    , first_event           :: warbd_type:timestamp()
    , prev_event            :: warbd_type:timestamp()
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


-define(REPORT_PERIOD, 1*60*1000).
-define(OLDEST_CHECK_PERIOD, 15*60*1000).


init([World]) ->
    lager:notice("Starting presence service for ~p", [World]),
    EvtChannel = warbd_channel:new(),
    
    PresenceTable = ets:new( table_name(World, presence), [protected, set, named_table, {read_concurrency, true}, {keypos, 1}]),
    LoginTable = ets:new( table_name(World, login), [protected, ordered_set, named_table, {read_concurrency, true}, {keypos, 1}]),
    
    publisher:subscribe(EvtChannel, warbd_channel:player_event(World)),
    publisher:subscribe(EvtChannel, warbd_channel:player_status(World)),
    
    timer:send_interval(?REPORT_PERIOD, {period_interval}),
    timer:send_interval(?OLDEST_CHECK_PERIOD, {oldest_check}),
    
    { ok
    , #state{ evtchannel = EvtChannel
            , presence_table = PresenceTable
            , login_table = LoginTable
            , world = World
            , first_event = xtime:unix_time()
            , prev_event = xtime:unix_time()
            , oldest_player = undefined
            , stats = #interval_stats{ total = 0
                                     , minmax = {0, 0}
                                     , event_count = {0, 0, 0}
                                     , faction = { new_faction_stats(0)
                                                 , new_faction_stats(0)
                                                 , new_faction_stats(0)
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


handle_info( {period_interval}
           , #state{ world = World
                   , stats = #interval_stats{ total = Total
                                            , minmax = Minmax
                                            , event_count = {EvtIn, EvtOut, EvtDel}
                                            , faction = {VS, NC, TR}
                                            } = Stats
                   , first_event = First
                   , oldest_player = Oldest } = State) ->

    OldestInfo =    case Oldest of
                        undefined               -> "none"
                    ;   {OldId, OldestLogin}    ->
                            xstring:format("~s ~s ago", [warbd_player_info:name(OldId), text:timesince(OldestLogin)])
                    end,
                    
    TotalStr = format_range(Total, Minmax),
                    
    lager:notice( "Population(~p): ~s  uptime ~s, oldest ~s, events: {~p, ~p, ~p}, by faction  ~s, ~s, ~s"
                , [ World, TotalStr, text:timesince(First), OldestInfo
                  , EvtIn, EvtOut, EvtOut - EvtDel
                  , format_faction(faction_vs, Total, VS)
                  , format_faction(faction_nc, Total, NC)
                  , format_faction(faction_tr, Total, TR)]),
                    
    { noreply
    , State#state{ stats = reset_interval_stats(Stats)
                 }
    };
    
    
handle_info( {oldest_check}
           , #state{ oldest_player = Oldest } = State) ->
           
    case Oldest of
        undefined       -> ok
    ;   {PlayerId, _}   -> warbd_query:request_player_status(PlayerId)
    end,
    
    {noreply, State};
    
    
handle_info( {login, PlayerId, World, Faction, Timestamp}
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
    , NState#state{ stats = update_interval_stats(login, Faction, Stats)
                  , prev_event = Timestamp
                  }
    };
    
    
handle_info( {logout, PlayerId, World, Faction, Timestamp}
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
                        logout
                end,
    
    NState =    case Oldest of
                    {PlayerId, _}   ->
                        case ets:first(LTab) of
                            {NwL, NwP}      ->
                                warbd_query:request_player_status(NwP),
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
    , MState#state{ stats = update_interval_stats(Action, Faction, Stats)
                  , prev_event = Timestamp
                  }
    };

    
handle_info( {online, PlayerId, World, Faction, Online}
           , #state{ world = World
                   , presence_table = PTab } = State) ->
                   
    InTable =   case ets:lookup(PTab, PlayerId) of
                    []          -> false
                ;   [{_, _}]    -> true
                end,
                
    lager:info("online status for ~p.. table: ~p, api: ~p ", [PlayerId, InTable, Online]),
                
    case {InTable, Online} of
        {X, X}          -> ok   % table agrees with API
        
                                % send a fake event to make the table agree with the API
    ;   {true, false}   -> self() ! {logout, PlayerId, World, Faction, xtime:unix_time()}
    ;   {false, true}   -> self() ! {login, PlayerId, World, Faction, xtime:unix_time()}
    end,
    
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


-spec adjust( login | ghost | logout, non_neg_integer() ) -> non_neg_integer().

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


new_faction_stats(X) ->
    #faction_stats{ count = X, minmax = {X, X} }.
    

%%%%% ------------------------------------------------------- %%%%%


update_evt_count(login, {Logins, Logouts, Deleted}) -> {Logins + 1, Logouts, Deleted};
update_evt_count(ghost, {Logins, Logouts, Deleted}) -> {Logins, Logouts + 1, Deleted};
update_evt_count(logout, {Logins, Logouts, Deleted}) -> {Logins, Logouts + 1, Deleted + 1}.


%%%%% ------------------------------------------------------- %%%%%


-spec update_minmax( non_neg_integer(), {non_neg_integer(), non_neg_integer()} ) -> {non_neg_integer(), non_neg_integer()}.

update_minmax(Value, {Min, Max}) ->
    if
        Value < Min -> {Value, Max}
    ;   Value > Max -> {Min, Value}
    ;   true        -> {Min, Max}
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
-spec update_interval_stats( login | ghost | logout, warbd_type:faction(), #interval_stats{} ) -> #interval_stats{}.

update_interval_stats( What, Faction
                     , #interval_stats{ total = Total
                                      , minmax = Minmax
                                      , event_count = EvtCount
                                      , faction = Faction_stats }) ->
    NewTotal = adjust(What, Total),
    NewFaction = update_faction_stats(What, faction_get(Faction, Faction_stats)),

    #interval_stats{ total = NewTotal
                   , minmax = update_minmax(NewTotal, Minmax)
                   , event_count = update_evt_count(What, EvtCount)
                   , faction = faction_set(Faction, NewFaction, Faction_stats)
                   }.
          
                        
%%%%% ------------------------------------------------------- %%%%%


-spec update_faction_stats( login | ghost | logout, #faction_stats{} ) -> #faction_stats{}.
                       
update_faction_stats( What
                    , #faction_stats{ count = Count
                                    , minmax = Minmax } ) ->                
    NewCount = adjust(What, Count),
    #faction_stats{ count = NewCount
                  , minmax = update_minmax(NewCount, Minmax)
                  }.


%%%%% ------------------------------------------------------- %%%%%


reset_interval_stats( #interval_stats{ total = Total
                                     , faction = {VS, NC, TR} } ) ->               
    #interval_stats{ total = Total
                   , minmax = {Total, Total}
                   , event_count = {0, 0, 0}
                   , faction = { new_faction_stats( get_count(VS) )
                               , new_faction_stats( get_count(NC) )
                               , new_faction_stats( get_count(TR) )
                               }
                   }.
                   

%%%%% ------------------------------------------------------- %%%%%


get_faction_counts( #interval_stats{ faction = {VS, NC, TR} } ) ->
    { get_count(VS)
    , get_count(NC)
    , get_count(TR)
    }.
    
get_count( #faction_stats{ count = Count } ) -> Count.


%%%%% ------------------------------------------------------- %%%%%


format_range(X, {Min, Max}) ->
    xstring:format("~p (~p .. ~p)", [X, Min, Max]).
    
    
%%%%% ------------------------------------------------------- %%%%%


format_faction( Faction, 0, #faction_stats{} ) ->
    xstring:format( "~s: 0.0%  0 (0 .. 0)", [ faction_get(Faction, {"VS", "NC", "TR"}) ] );

    
format_faction( Faction
              , Total
              , #faction_stats{ count = Count
                              , minmax = Minmax } ) ->
    xstring:format( "~s: ~.1f%  ~s"
                  , [ faction_get(Faction, {"VS", "NC", "TR"})
                    , Count / Total * 100.0
                    , format_range(Count, Minmax) ] ).

    
