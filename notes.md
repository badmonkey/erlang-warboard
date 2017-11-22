# erlang-warboard
Planetside2 Leaderboard


=== Topics and Messages ===

== Sent by war_specific_source[world] ==

## player/<world>/event ##
    { login_raw | logout_raw
    , PlayerId      :: binary()
    , World         :: warbd_type:world()
    , Timestamp     :: war_type:timestamp()
    }

    
== Sent by war_global_source ==
    
## server/<world>/status ##
    { heartbeat
    , World         :: warbd_type:world()
    , Online        :: boolean()
    , Timestamp     :: war_type:timestamp()
    , Interval      :: xtime:seconds()
    }   

## server/event ##
    { Type          :: short_period_timer | check_period_timer | long_period_timer
                     % 2mins, 15mins, 1hr
    , Timestamp     :: war_type:timestamp()
    , Interval      :: xtime:seconds()
    }    

-----------------------------------------------------------------------------------------------------    
== Sent by war_presence[world] ==

## player/<world>/<faction>/status ##
    { playtime
    , PlayerId      :: war_type:player_id()
    , World         :: war_type:world()
    , Faction       :: war_type:faction()
    , Duration      :: xtime:seconds()
    , Oldest        :: boolean()
    }
    
    { oldest
    , PlayerId      :: war_type:player_id()
    , World         :: war_type:world()
    , Faction       :: war_type:faction()
    , Interval      :: xtime:seconds()
    }

## population/<world>/event ##
    { pop_change
    , World         :: war_type:world()
    , Timestamp     :: war_type:timestamp()
    , Interval      :: xtime:seconds()
    , Events        :: {in, out, ghost}
    , Total         :: {Value, Min, Max}
    , Faction       :: #{ warbd_type:faction() => {Value, Min, Max} }
    }    
    
    
== Sent by war_api_query ==
    
## player/<world>/<faction>/info ##
    #db_player_info{...}
    #db_player_stats{...}


== Sent by war_player_info[world] ==
    
    
=====================================

    

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




## outfit/<world>/info



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
    
    
    
    
