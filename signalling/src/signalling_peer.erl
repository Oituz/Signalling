-module(signalling_peer).
-behaviour(gen_server).

%% API
-export([start_link/1,start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,join_meeting/2]).
-record(state, {
    id,
    sfu_pid=undefined,
    caller_pid,
    fsm_pid,
    candidates
}).


start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).

-spec join_meeting(PeerId::integer()|string(),MeetingId::integer()|string())->{ok,PeerPid::pid()}.
join_meeting(PeerId,MeetingId)->
    {ok,PeerPid}=register_service:get_peer(PeerId),
    gen_server:call(PeerPid,{join_meeting,MeetingId}).
    
-spec send_fsm_to_peer_message(Pid::pid(),Message::any())->ok.
send_fsm_to_peer_message(Pid,Message)->
    gen_server:cast(Pid,{fsm_message,Message}).    

init(Args) ->
    #{id :=Id}=Args,
    {ok, #state{id=Id}}.


handle_cast({fsm_message,fetch_own_candidates},State=#state{caller_pid = CallerPid})->
    CallerPid ! fetch_own_candidates,
    {noreply,State};
handle_cast({fsm_message,{send_offer,Candidates}},State=#state{fsm_pid = FsmPid})->
    signalling_fsm:send_signalling_message(FsmPid,acknowledge),
    {noreply,State#state{candidates = Candidates}};
handle_cast({fsm_message,{send_offer,Candidates}},State=#state{fsm_pid = FsmPid})->
    signalling_fsm:send_signalling_message(FsmPid,acknowledge),
    {noreply,State#state{candidates = Candidates}};
handle_cast({fsm_message,ready_for_streaming},State=#state{sfu_pid = SfuPid,candidates = Candidates})->
    ok=signalling_sfu:connect(SfuPid,#{peer_pid=>self(),candidates=>Candidates}),
    State#state.caller_pid ! connected,
    {noreply,State};
    
handle_cast({candidates,Candidates},State=#state{fsm_pid = FsmPid})->
    signalling_fsm:send_signalling_message(FsmPid,{candidates,Candidates}),
    {noreply,State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({join_meeting,MeetingId},_From,State=#state{fsm_pid=undefined})->
    {ok,SfuPid}=register_service:get_sfu(MeetingId),
    FsmData=#{peer_process_pid=>self(),notify_pid=>self()},
    {ok,FsmPid}=signalling_fsm_sup:start(FsmData),
    {noreply,State#state{fsm_pid = FsmPid,caller_pid = _From,sfu_pid = SfuPid}};
   

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


       
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
