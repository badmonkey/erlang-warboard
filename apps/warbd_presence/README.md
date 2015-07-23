
Project warbd_presence 1.0.0

player online status + population events


= Events Listen =
player/<world>/<faction>/event
    { login | logout
    , PlayerId      :: warbd_type:player_id()
    , World         :: warbd_type:world()
    , Faction       :: warbd_type:faction()
    , Timestamp     :: warbd_type:timestamp()}


= Events Emitted =

population/<world>/<faction>/event
    { pop_change, World, Faction, Timestamp, IntervalSecs,
        #{ logins   => {ins, outs}
         , unique   => {ins, outs}
         , range    => {min, max}
         , avg      => Average
         , current  => Current}
    }

    
= Public API =

population(World) -> {VS, NC, TR}
info(Player) -> {online, World, OnSince} | offline
oldest_player(World) -> {PlayerId, Timestamp}


= ETS tables =

#presence{PlayerId, World, Timestamp}