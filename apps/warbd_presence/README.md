
Project warbd_presence 1.0.0

player online status + population events


    
= Public API =

population(World) -> {VS, NC, TR}
info(Player) -> {online, World, OnSince} | offline
oldest_player(World) -> {PlayerId, Timestamp}


= ETS tables =

#presence{PlayerId, World, Timestamp}