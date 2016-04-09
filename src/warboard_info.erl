
-module(warboard_info).


-export([ census_id/0, world/1, zone/1, faction/1
        , faction_name/1, timestamp/1, world_tag/1
        , battlerank/1, brackets/2]).


%%%%% ------------------------------------------------------- %%%%%


census_id() ->
    "s:warboard".


%%%%% ------------------------------------------------------- %%%%%


-spec world( binary() | string() | integer() ) -> warbd_type:world() | offline | type:exception().

world(B) when is_binary(B) ->
    world( xerlang:binary_to_integer(B) );

world(L) when is_list(L) ->
    world( list_to_integer(L) );
    
world(I) when is_integer(I) ->
    case I of
        0   -> offline
    ;   1   -> connery
    ;   10  -> miller
    ;   13  -> cobalt
    ;   17  -> emerald
    ;   19  -> jaeger
    ;   25  -> briggs
    ;   _   -> throw({error, unknown_world_id})
    end.


%%%%% ------------------------------------------------------- %%%%%


world_tag(<<"EventServerEndpoint_Connery_1">>)  -> connery;
world_tag(<<"EventServerEndpoint_Miller_10">>)  -> miller;
world_tag(<<"EventServerEndpoint_Cobalt_13">>)  -> cobalt;
world_tag(<<"EventServerEndpoint_Emerald_17">>) -> emerald;
world_tag(<<"EventServerEndpoint_Jaeger_19">>)  -> jaeger;
world_tag(<<"EventServerEndpoint_Briggs_25">>)  -> briggs;

world_tag(_) -> throw({error, unknown_world_tag}).

    
%%%%% ------------------------------------------------------- %%%%%


-spec zone( binary() | string() | integer() ) -> warbd_type:zone() | type:exception().

zone(B) when is_binary(B) ->
    zone( xerlang:binary_to_integer(B) );

zone(L) when is_list(L) ->
    zone( list_to_integer(L) );
    
zone(I) when is_integer(I) ->
    case I of
        2   -> indar
    ;   4   -> hossin
    ;   6   -> amerish
    ;   8   -> esamir
    ;   _   -> throw({error, unknown_zone_id})
    end.

    
%%%%% ------------------------------------------------------- %%%%%


-spec faction( binary() | string() | integer() ) -> warbd_type:faction() | type:exception().

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
    
    
faction_name(faction_vs) -> "VS";
faction_name(faction_nc) -> "NC";
faction_name(faction_tr) -> "TR".    
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec timestamp( binary() ) -> warbd_type:timestamp().

timestamp(B) when is_binary(B) ->
    xerlang:binary_to_integer(B).


%%%%% ------------------------------------------------------- %%%%%



battlerank(Score)
        when Score < 18868950  ->
    undefined;
    
battlerank(Score) ->
    length( lists:takewhile( fun(X) -> Score >= X end, extended_xp_brackets() ) ) + 99.

 
% New 100-120 battle ranks 
%18868950, 19812397, 20755845, 21699292, 22642740, 23586187, 24529635,
%25473082, 26416530, 27359977, 28303425, 29246872, 30190320, 31133767,
%32077215, 33020662, 33964110, 34907557, 35851005, 36794452, 37737900
    
% 
% Based off AzureProdigy formula  y = 5E-05x6 - 0.0113x5 + 0.9089x4 - 21.787x3 + 1195.7x2 - 9356.8x + 22558  + 1722072  
% Output for  100 <= x <= 300
%
extended_xp_brackets() ->    
    [18868950,19442131,20039009,20660957,21309419,21985911,22692025,23429431,24199880
    ,25005204,25847324,26728243,27650061,28614966,29625245,30683281,31791559,32952669
    ,34169307,35444279,36780502,38181009,39648952,41187604,42800363,44490752,46262427
    ,48119176,50064925,52103739,54239826,56477540,58821387,61276024,63846266,66537086
    ,69353621,72301175,75385224,78611415,81985574,85513707,89202008,93056856,97084822
    ,101292677,105687387,110276125,115066270,120065413,125281360,130722136,136395992
    ,142311402,148477076,154901958,161595230,168566320,175824906,183380915,191244534
    ,199426208,207936651,216786845,225988048,235551796,245489909,255814493,266537952
    ,277672981,289232582,301230060,313679036,326593442,339987535,353875897,368273439
    ,383195411,398657401,414675344,431265526,448444587,466229530,484637724,503686908
    ,523395199,543781093,564863476,586661625,609195215,632484324,656549437,681411457
    ,707091702,733611917,760994279,789261398,818436328,848542571,879604080,911645270
    ,944691017,978766672,1013898059,1050111486,1087433748,1125892137,1165514442
    ,1206328961,1248364502,1291650394,1336216488,1382093169,1429311356,1477902514
    ,1527898656,1579332350,1632236729,1686645494,1742592919,1800113862,1859243768
    ,1920018677,1982475231,2046650679,2112582885,2180310333,2249872137,2321308045
    ,2394658445,2469964376,2547267529,2626610262,2708035596,2791587233,2877309556
    ,2965247637,3055447249,3147954864,3242817671,3340083574,3439801203,3542019923
    ,3646789840,3754161804,3864187424,3976919070,4092409883,4210713780,4331885463
    ,4455980430,4583054974,4713166200,4846372027,4982731197,5122303283,5265148696
    ,5411328695,5560905393,5713941764,5870501654,6030649784,6194451765,6361974099
    ,6533284192,6708450359,6887541834,7070628777,7257782284,7449074392,7644578092
    ,7844367331,8048517027,8257103072,8470202345,8687892717,8910253059,9137363255
    ,9369304206,9606157842,9848007126,10094936068,10347029731,10604374239,10867056789
    ,11135165656,11408790204,11688020893,11972949292,12263668083,12560271074,12862853204
    ,13171510558,13486340369,13807441033,14134912114,14468854355,14809369689,15156561246
    ,15510533362,15871391590].
    
    
brackets(L, H) ->
    lager:notice("~p", [ [trunc(azure(X)) || X <- lists:seq(L,H) ] ]).
    
azure(X) ->
      5.0e-5 *math:pow(X,6)
    - 0.0113 *math:pow(X,5)
    + 0.9089 *math:pow(X,4)
    - 21.787 *math:pow(X,3)
    + 1195.7 *math:pow(X,2)
    - 9356.8 * X
    + 22558  + 1722072.
