


-record(db_player_info, 
    { player_id             :: war_type:player_id()
    , name                  :: string()
    , world                 :: war_type:world()
    , faction               :: war_type:faction()
    , last_update           :: war_type:timestamp()
    }).
    
    
-record(count_stats_type,
    { value                 :: non_neg_integer()
    , monthly               :: non_neg_integer()
    , weekly                :: non_neg_integer()
    , daily                 :: non_neg_integer()
    , one_life_max          :: non_neg_integer()
    }).
    
    
-record(db_player_stats,
    { player_id             :: war_type:player_id()
    , last_update           :: war_type:timestamp()
    , last_login            :: war_type:timestamp()
    , battle_rank           :: pos_integer()
    , login_count           :: pos_integer()
    , minutes_played        :: non_neg_integer()
    , score                 :: #count_stats_type{}
    , kills                 :: #count_stats_type{}
    , deaths                :: #count_stats_type{}
    }).
    
    
