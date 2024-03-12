-module(sfu).
-behaviour(gen_server).
-include("../include/rtp.hrl").
-include("../include/domain.hrl").
%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([update_candidates/2,update_track/2,add_track/3,remove_track/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {id,peers,sessions}).

-record(peer_data,{
    id,
    pid,
    candidates,
    tracks
}).

-record(session_data,{
    ssrc,
    trackname,
    rtp_session_pid,
    source_peer_id,
    subscribers
}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------------------------------------------- PUBLIC API -------------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec connect(SfuPid::pid(),ConnectParams::rtp:connect_params())->ok | {error,Reason::any()}.
connect(Pid,ConnectData)->
    gen_server:call(Pid,{connect,ConnectData}).

-spec update_candidates(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),candidates=>Candidates::[rtp:candidate()]})->ok.
update_candidates(SfuPid,_PeerData=#{peer_id:=PeerId,candidates:=Candidates})->
    gen_server:cast(SfuPid,{update_candidates,#{peer_id=>PeerId,candidates=>Candidates}}).

-spec update_track(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),ssrc=>SSRC::integer(),track=>rtp:track()})->ok.
update_track(SfuPid,_Data=#{ssrc:=SSRC,track:=Track})->
    gen_server:cast(SfuPid,{update_track,#{ssrc=>SSRC,track=>Track}}).

-spec add_track(SfuPid::pid(),PeerId::integer(),Track::rtp:track())->ok.
add_track(SfuPid,PeerId,Track)->
    gen_server:cast(SfuPid,{add_track,PeerId,Track}).

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
    {ok, #state{id=Id, peers=dict:new()},sessions=dict:new(),session_subscribers_map=dict:new()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------------------------------------------Callbacks----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compute_peer_data(ConnectParams,_State)->
    #{ peer_pid:=PeerPid,
        peer_id:=PeerId,
        rtp_params:=#rtp_params{
            candidates=Candidates,
            tracks=Tracks}}
    =ConnectParams,
    #peer_data{id = PeerId,pid = PeerPid, candidates=Candidates, tracks = Tracks}.
generate_rtp_sessions(Data=#peer_data{tracks = Tracks})->
    Sessions=[generate_rtp_session(#{peer_id=>Data#peer_data.id,peer_pid=>Data#peer_data.pid,track=>Track})||Track<-Tracks],
    {ok,Sessions}.
generate_rtp_session(InputSessionData=#{peer_id := PeerId,track := Track=#track{}})->
    SSRC=generate_ssrc(),
    {ok,RTPSessionPid}=rtp_session:start(#rtp_session_start_params{source_peer_id=PeerId,ssrc=SSRC,track=Track}),
    InputSessionData=#session_data{ssrc=SSRC, source_peer_id = PeerId, trackname = Track#track.id,rtp_session_pid = RTPSessionPid,subscribers = []},
    InputSessionData.

generate_ssrc()->
    rand:uniform().

subscribe_peer_to_session(_=#peer_data{pid = Pid,id =PeerId},SessionData=#session_data{rtp_session_pid = RtpSessionPid,subscribers = Subscribers})->
    rtp_session:add_subscriber(RtpSessionPid,#{peer_id=>Pid}).

unsubscribe_peer_from_session()->ok.

handle_call({connect,ConnectParams=#connect_params{peer_id=PeerId}},_From,State)->
    NewPeer=compute_peer_data(ConnectParams,State),
    NewRTPSessionsList=generate_rtp_sessions(NewPeer),
    [subscribe_peer_to_session(Peer,RTPSession)||Peer<-State#state.peers,RTPSession<-NewRTPSessionsList],
    NewRTPSessionMap=store_new_rtp_sessions(NewRTPSessionsList, State#state.sessions),
    Reply={ok,#{ssrc_session_map=>NewRTPSessionMap}},
    {reply,Reply,State#state{peers = dict:store(PeerId, NewPeer, State#state.peers),sessions = NewRTPSessionMap}};

%------------------------------------------------------------------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_candidates,#{peer_id:=PeerId,candidates:=Candidates}},State)->
    case dict:find(PeerId, State#state.peers) of
        error -> io:format("Could not find peer ~p",[PeerId]),
                 {noreply,State};
                 
        {ok,PeerData}-> NewPeerData=inner_update_candidates(Candidates, PeerData),
                        NewPeers=dict:store(PeerId, NewPeerData,State#state.peers),
                        {noreply,State#state{peers = NewPeers}}
    end;
   

handle_cast({add_track,#{peer_id:=PeerId,track:=Track}},State)->
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

store_new_rtp_sessions(NewSessionList,ExistingSessions)->
    NewSessionMap=lists:foldl(
        fun(Item,Dict)->
            #session_data{ssrc = SSRC}=Item,
            dict:store(SSRC, Item, Dict)
        end,
        ExistingSessions, NewSessionList),
    NewSessionMap.

inner_update_candidates(Candidates,PeerData)->
    PeerData#peer_data{candidates = Candidates}.




