-module(sfu).
-behaviour(gen_server).
-include("../include/rtp.hrl").
-include("../include/domain.hrl").
%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([update_candidates/2,update_track/2,add_track/2,remove_track/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
        id,
        peers,
        peer_monitor_map,
        ssrc_session_map}).

-record(peer_state,{
    id,
    pid,
    candidates,
    tracks
}).

-record(rtp_session_state,{
    ssrc,
    trackname,
    rtp_session_pid,
    source_peer_id,
    subscribers
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------------------------------------------- PUBLIC API -------------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec connect(SfuPid::pid(),ConnectParams::domain:connect_params())->{ok,domain:connect_response()} | {error,Reason::any()}.
connect(Pid,ConnectData)->
    gen_server:call(Pid,{connect,ConnectData}).

-spec update_candidates(SfuPid::pid(),UpdateCandidatesParams::#update_candidates_params{})->ok.
update_candidates(SfuPid,UpdateCandidatesParams)->
    gen_server:cast(SfuPid,{update_candidates,UpdateCandidatesParams}).

-spec update_track(SfuPid::pid(),UpdateTrackParams::#update_track_params{})->ok.
update_track(SfuPid,UpdateTrackParams)->
    gen_server:cast(SfuPid,UpdateTrackParams).

-spec add_track(SfuPid::pid(),AddTrackParams::domain:add_track_params())->ok.
add_track(SfuPid,AddTrackParams)->
    gen_server:cast(SfuPid,{add_track,AddTrackParams}).

-spec remove_track(SfuPid::pid(),SSRC::any())->ok.
remove_track(SfuPid,SSRC)->
    gen_server:cast(SfuPid,{remove_track,SSRC}).
start(Args)->
    sfu_sup:start(Args).
stop(Name) ->
    gen_server:call(Name, stop).

start_link(SfuData) ->
    gen_server:start_link(?MODULE, SfuData, []).

init(_=#{id:=Id}) ->
    {ok, #state{id=Id, peers=dict:new()},ssrc_session_map=dict:new(),session_subscribers_map=dict:new(),peer_monitor_map=dict:new()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------------------------------------Callbacks----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compute_peer_data(ConnectParams::domain:connect_params(),State::#state{})->{ok,#peer_state{}} | already_exists.
compute_peer_data(_=#connect_params{peer_id = PeerId,rtp_params = RtpParams},_State)->
    case dict:find(PeerId,_State#state.peers) of
        {ok,_} -> already_exists;
        error->
                {ok,PeerPid}=register_service:get_peer(PeerId),
                #rtp_params{candidates=Candidates,tracks=Tracks}=RtpParams,
                {ok,#peer_state{id = PeerId,pid = PeerPid, candidates=Candidates, tracks = Tracks}}
end.


generate_rtp_sessions(Data=#peer_state{tracks = Tracks})->
    Sessions=[generate_rtp_session(#{peer_id=>Data#peer_state.id,peer_pid=>Data#peer_state.pid,track=>Track})||Track<-Tracks],
    {ok,Sessions}.
generate_rtp_session(InputSessionData=#{peer_id := PeerId,track := Track=#track{}})->
    SSRC=generate_ssrc(),
    {ok,RTPSessionPid}=rtp_session:start(#rtp_session_start_params{source_peer_id=PeerId,ssrc=SSRC,track=Track}),
    InputSessionData=#rtp_session_state{ssrc=SSRC, source_peer_id = PeerId, trackname = Track#track.id,rtp_session_pid = RTPSessionPid,subscribers = []},
    InputSessionData.

generate_ssrc()->
    rand:uniform().
subscribe_peer_to_session(_=#peer_state{pid = Pid,id =PeerId},SessionData=#rtp_session_state{ssrc = Ssrc,rtp_session_pid = RtpSessionPid,subscribers = Subscribers})->
    case lists:filter(fun(P)->P=:=PeerId end, Subscribers) of
        [_] -> SessionData;
        [] ->   case rtp_session:add_subscriber(RtpSessionPid,#add_subscriber_params{peer_id = PeerId,peer_pid = Pid}) of
                    ok -> SessionData#rtp_session_state{subscribers = [PeerId|Subscribers]};
                    _ ->    io:format("Could not subscribe Peer ~p to Session ~p",[PeerId,Ssrc]),
                            SessionData
                end
    end.

unsubscribe_peer_from_session()->ok.


handle_connect(NewPeer,State)->
   
    RTPSessions=generate_rtp_sessions(NewPeer),
    RTPSessionsWithSubscribers=[subscribe_peer_to_session(Peer,RTPSession)||Peer<-State#state.peers,RTPSession<-RTPSessions],
    NewRTPSessionMap=add_new_tracks_to_ssrc_session_map(RTPSessionsWithSubscribers, State#state.ssrc_session_map),
    {ok,#{ssrc_session_map=>NewRTPSessionMap,new_peer=>NewPeer}}.

handle_call({connect,ConnectParams},_From,State)->
    NewPeer=compute_peer_data(ConnectParams,State),
    {ok,#{ssrc_session_map:=RTPSessionMap,newpeer:=#peer_state{id = Id}}}=handle_connect(NewPeer,State),
    {reply,{ok,#{ssrc_session_map=>RTPSessionMap}},State#state{peers = dict:store(Id, NewPeer, State#state.peers),ssrc_session_map = RTPSessionMap}};



%------------------------------------------------------------------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_candidates,#update_candidates_params{peer_id=PeerId,candidates=Candidates}},State)->
    case dict:find(PeerId, State#state.peers) of
        error -> io:format("Could not find peer ~p",[PeerId]),
                 {noreply,State};
                 
        {ok,PeerData}-> NewPeerData=inner_update_candidates(Candidates, PeerData),
                        NewPeers=dict:store(PeerId, NewPeerData,State#state.peers),
                        {noreply,State#state{peers = NewPeers}}
    end;
   

handle_cast({add_track,#add_track_params{peer_id=PeerId,track=Track}},State)->
    {noreply,State}; 
        
handle_cast({remove_track,SSRC},State)->
    {noreply,State};        

handle_cast({update_track,#{ssrc:=SSRC,track:=Track}},State)->
    {noreply,State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_new_tracks_to_ssrc_session_map(NewSessionList,ExistingSessions)->
    NewSessionMap=lists:foldl(
        fun(Item,Dict)->
            #rtp_session_state{ssrc = SSRC}=Item,
            dict:store(SSRC, Item, Dict)
        end,
        ExistingSessions, NewSessionList),
    NewSessionMap.

inner_update_candidates(Candidates,PeerData)->
    PeerData#peer_state{candidates = Candidates}.




