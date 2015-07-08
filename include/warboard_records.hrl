


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
    
    
