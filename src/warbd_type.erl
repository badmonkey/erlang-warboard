
-module(warbd_type).


-export_type([ player_id/0, faction/0]).


%%%%% ------------------------------------------------------- %%%%%


-type player_id() :: integer().
-type faction() :: faction_vs | faction_nc | faction_tr.
