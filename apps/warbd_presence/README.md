
Project warbd_presence 1.0.0

player online status + population events


= Events Listen =
player/<faction>/event
    {login|logout, PlayerId, Faction, Timestamp}


= Events Emitted =

population/<faction>/event
    { pop_change, Faction, IntervalSecs,
        #{ logins   => {ins, outs}
         , unique   => {ins, outs}
         , range    => {min, max}
         , avg      => Average
         , current  => Current}
    }

    
= Public API =

population() -> {VS, NC, TR}
info(Player) -> {online, OnSince} | undefined
oldest_player() -> {PlayerId, Timestamp}


= ETS tables =

#presence{PlayerId, Timestamp}