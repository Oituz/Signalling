-module(signalling_fsm).
-behaviour(gen_statem).

-export([start/1]).
-export([stop/0, start_link/1,send_signalling_message/2,get_state/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

-define(Name, ?MODULE).
%% API
-record(state, {
    current_state=idle,
    local_ice_candidates=[],
    
    notify_pid,
    peer_process_pid
}).

%-----------------------API---------------------------------------

-spec start(FsmData::{init_data,PeerProcessPid::pid(),NotifyPid::pid()})-> {ok,Pid::pid()} | {error,Reason::any()}.
start(FsmData)->
    signalling_fsm_sup:start(FsmData).

-spec get_state(Pid::pid())->{ok,State::atom()}.

get_state(Pid)->
    gen_statem:call(Pid,get_state).

-spec send_signalling_message(Pid::pid(),Message::any())->ok.


send_signalling_message(Pid,Message)->
    gen_statem:cast(Pid, {signalling_message,Message}).



stop() ->
    gen_statem:stop(?MODULE).

start_link(FsmData) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [FsmData], []).

init(#{peer_process_pid:=PeerProcessPid,notify_pid:=NotifyPid}) ->
    gen_statem:send_event(self(), fetch_own_candidates),
    {ok, idle, #state{notify_pid = NotifyPid,peer_process_pid = PeerProcessPid}}.

%----------------------------------------------------------------
%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
    handle_event_function.
handle_event(get_state,CurrentFsmState,State=#state{peer_process_pid = PeerProcessPid},_Data)->
    signalling_peer:send_fsm_to_peer_message(PeerProcessPid,{current_state,CurrentFsmState}),
    {keep_state,CurrentFsmState,State};


handle_event(fetch_own_candidates,idle,State=#state{peer_process_pid = PeerProcessPid},Data)->
    signalling_peer:send_fsm_to_peer_message(PeerProcessPid,fetch_own_candidates),
    {next_state,waiting_for_own_candidates,State,Data};
handle_event({signalling_message,{candidates,Candidates}},waiting_for_own_candidates,#state{peer_process_pid = PeerProcessPid}=State,_Data)->
    NewState=State#state{local_ice_candidates = Candidates},
    signalling_peer:send_fsm_to_peer_message(PeerProcessPid,{send_offer,Candidates}),
    {next_state,waiting_for_ack,NewState};
handle_event({signalling_message,acknowledge},waiting_for_ack,#state{peer_process_pid = PeerProcessPid}=State,Data)->
    signalling_peer:send_fsm_to_peer_message(PeerProcessPid,ready_for_streaming),
    {next_state,connection_established,State};

handle_event(enter, _OldState, _State, _Data) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.



