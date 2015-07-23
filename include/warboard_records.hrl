


-record(db_player_info, 
    { player_id             :: warbd_type:player_id()
    , name                  :: string()
    , world                 :: warbd_type:world()
    , faction               :: warbd_type:faction()
    , last_update           :: warbd_type:timestamp()
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
    , last_update           :: warbd_type:timestamp()
    , last_login            :: warbd_type:timestamp()
    , battle_rank           :: non_neg_integer()
    , login_count           :: pos_integer()
    , minutes_played        :: non_neg_integer()
    , score                 :: #count_stats_type{}
    , kills                 :: #count_stats_type{}
    , deaths                :: #count_stats_type{}
    }).
    
    
