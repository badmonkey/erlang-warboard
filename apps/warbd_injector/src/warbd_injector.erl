
-module(warbd_injector).
-vsn("1.0.0").

-behaviour(websock_client).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2, websock_info/0, handle_frame/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { evtchannel
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    websock_client:start_link(?SERVER, ?MODULE).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


websock_info() ->
    [ {url, "wss://push.planetside2.com/streaming?environment=ps2&service-id=s:example"}
    ].
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting websock here"),
    {ok, #state{ evtchannel = warbd_channel:new() }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_frame({system, up}, #state{} = State) ->
    { ok
    , #{ <<"service">> => <<"event">>
       , <<"action">> => <<"subscribe">>
       , <<"worlds">> => [<<"25">>]
       , <<"eventNames">> => [<<"PlayerLogin">>, <<"PlayerLogout">>]
       }
    , State};


handle_frame(Msg, #state{} = State) ->
    lager:info("warbd_injector:frame ~p", [Msg]),
    publisher:notify(State#state.evtchannel, [player, tr, event], {login, "Hello World"}),
    {ok, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, #state{} = State) ->
    lager:info("warbd_injector:call UNKNOWN ~p", [_Request]),
    {reply, ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, #state{} = State) ->
    lager:info("warbd_injector:cast UNKNOWN ~p", [_Msg]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, #state{} = State) ->
    lager:info("warbd_injector:info UNKNOWN ~p", [_Info]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


