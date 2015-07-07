
-module(warbd_presence).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2]).
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    lager:info("Starting presence"),
    EvtChannel = warbd_channel:new(),
    
    publisher:subscribe(EvtChannel, warbd_channel:player_event()),
    
    {ok, #state{ evtchannel = EvtChannel }}.

    
%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    lager:error("call STOPPED ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:error("cast STOPPED ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    lager:warning("info UNKNOWN ~p", [_Info]),
    {noreply, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


