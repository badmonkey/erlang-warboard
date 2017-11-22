
-module(war_type).


-export_type([player_id/0, faction/0, world/0, zone/0, timestamp/0]).


%%%%% ------------------------------------------------------- %%%%%


-type player_id()   :: pos_integer().
-type faction()     :: faction_vs | faction_nc | faction_tr.
-type world()       :: connery | miller | cobalt | emerald | briggs | jaeger.
-type zone()        :: indar | esamir | amerish | hossin.
-type timestamp()   :: xtime:unix_timestamp().
