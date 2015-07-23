
-module(warboard_info).


-export([census_id/0, world/1, faction/1, timestamp/1]).


%%%%% ------------------------------------------------------- %%%%%


census_id() ->
    "s:warboard".


%%%%% ------------------------------------------------------- %%%%%


-spec world( binary() | string() | integer() ) -> warbd_type:world().

world(B) when is_binary(B) ->
    world( xerlang:binary_to_integer(B) );

world(L) when is_list(L) ->
    world( list_to_integer(L) );
    
world(I) when is_integer(I) ->
    case I of
        1   -> connery
    ;   10  -> miller
    ;   13  -> cobalt
    ;   17  -> emerald
    ;   19  -> jaeger
    ;   25  -> briggs
    ;   _   -> throw({error, unknown_world_id})
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec faction( binary() | string() | integer() ) -> warbd_type:faction().

faction(B) when is_binary(B) ->
    faction( xerlang:binary_to_integer(B) );

faction(L) when is_list(L) ->
    faction( list_to_integer(L) );
    
faction(I) when is_integer(I) ->
    case I of
        1   -> faction_vs
    ;   2   -> faction_nc
    ;   3   -> faction_tr
    ;   _   -> throw({error, unknown_faction_id})
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec timestamp( binary() ) -> warbd_type:timestamp().

timestamp(B) when is_binary(B) ->
    xerlang:binary_to_integer(B).

