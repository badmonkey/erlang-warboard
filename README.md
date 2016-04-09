# erlang-warboard
Planetside2 Leaderboard




=== Topics and Messages ===

## server/<world>/status ##
    { heartbeat
    , World         :: warbd_type:world()
    , Online        :: boolean()
    , Timestamp     :: warbd_type:timestamp()
    , Interval      :: xtime:seconds()
    }
% sent by injector    


    { online | offline
    , World...}
% sent by injector XXX


## server/event ##
    { Type          :: short_period_timer | check_period_timer | long_period_timer
                     % 2mins, 15mins, 1hr
    , Timestamp     :: warbd_type:timestamp()
    , Interval      :: xtime:seconds()
    }
% sent by injector
% listen for by query, presence



## player/<world>/<faction>/event ##
    { login | logout
    , PlayerId      :: warbd_type:player_id()
    , World         :: warbd_type:world()
    , Faction       :: warbd_type:faction()
    , Timestamp     :: warbd_type:timestamp()
    }
% sent by injector, presence
% listen for by presence


    { online
    , PlayerId      :: warbd_type:player_id()
    , World         :: warbd_type:world()
    , Faction       :: warbd_type:faction()
    , true | false
    }
% sent by query
% listen for by presence



## player/<world>/<faction>/info ##
    #db_player_info{}
% sent by query
% listen for by player_info
    
    
    #db_player_stats{}
% sent by query
% listen for by player_info



## player/<world>/<faction>/status ##
    { playtime
    , PlayerId      :: warbd_type:player_id()
    , World         :: warbd_type:world()
    , Faction       :: warbd_type:faction()
    , Duration      :: xtime:seconds()
    , Oldest        :: boolean()
    }
% sent by presence
    
    
    { oldest
    , PlayerId      :: warbd_type:player_id()
    , World         :: warbd_type:world()
    , Faction       :: warbd_type:faction()
    , Interval      :: xtime:seconds()
    }
% sent by presence


## outfit/<world>/info
    

    
## population/<world>/event ##
    { pop_change
    , World         :: warbd_type:world()
    , Timestamp     :: warbd_type:timestamp()
    , Interval      :: xtime:seconds()
    , Events        :: {in, out, ghost}
    , Total         :: {Value, Min, Max}
    , Faction       :: #{ warbd_type:faction() => {Value, Min, Max} }
    }
% sent by presence


facility/<world>/event
    {capture}
    {defend}
    
        // bad event old_faction_id = 0 => facility_id/zone_id/duration_held all wrong
    #{<<"duration_held">> => <<"1440944369">>
    ,<<"event_name">> => <<"FacilityControl">>
    ,<<"facility_id">> => <<"310612">>
    ,<<"new_faction_id">> => <<"1">>
    ,<<"old_faction_id">> => <<"0">>
    ,<<"outfit_id">> => <<"0">>
    ,<<"timestamp">> => <<"1440944369">>
    ,<<"world_id">> => <<"25">>
    ,<<"zone_id">> => <<"209125471">>}

    
    #{<<"duration_held">> => <<"4697">>
    ,<<"event_name">> => <<"FacilityControl">>
    ,<<"facility_id">> => <<"302020">>
    ,<<"new_faction_id">> => <<"3">>
    ,<<"old_faction_id">> => <<"2">>
    ,<<"outfit_id">> => <<"37519639440001347">>
    ,<<"timestamp">> => <<"1440945399">>
    ,<<"world_id">> => <<"25">>
    ,<<"zone_id">> => <<"4">>}
    
    
    #{<<"duration_held">> => <<"10575">>
    ,<<"event_name">> => <<"FacilityControl">>
    ,<<"facility_id">> => <<"296000">>
    ,<<"new_faction_id">> => <<"3">>
    ,<<"old_faction_id">> => <<"3">>
    ,<<"outfit_id">> => <<"0">>
    ,<<"timestamp">> => <<"1440945403">>
    ,<<"world_id">> => <<"25">>
    ,<<"zone_id">> => <<"4">>}    
    
    
    
    