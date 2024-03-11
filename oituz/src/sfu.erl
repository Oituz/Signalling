-module(sfu).
-behaviour(gen_server).
-include("../include/rtp.hrl").
%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([update_candidates/2,update_tracks/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {id,peers,sessions,session_subscribers_map}).

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
    source_peer_id
    
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------------------------------------------- PUBLIC API -------------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec connect(SfuPid::pid(),ConnectParams)->ok | {error,Reason::any()} 
    when
        ConnectParams::#{
            peer_id=>integer()|string()|binary(),
            rtp_params=>rtp:rtp_params()
        }.
connect(Pid,ConnectData)->
    gen_server:call(Pid,{connect,ConnectData}).

-spec update_candidates(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),candidates=>Candidates::[rtp:candidate()]})->ok.
update_candidates(SfuPid,_PeerData=#{peer_id:=PeerId,candidates:=Candidates})->
    gen_server:cast(SfuPid,{update_candidates,#{peer_id=>PeerId,candidates=>Candidates}}).

-spec update_track(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),ssrc=>SSRC::integer(),track=>rtp:track()})->ok.
update_track(SfuPid,_PeerData=#{peer_id:=PeerId,ssrc:=SSRC,track:=Track})->
    gen_server:cast(SfuPid,{update_track,#{peer_id=>PeerId,ssrc=>SSRC,track=>Track}}).
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


handle_call({connect,ConnectParams=#{peer_id:=PeerId}},_From,State)->
    NewPeer=compute_peer_data(ConnectParams,State),
    Sessions=generate_rtp_sessions(NewPeer),
    [subscribe_peer_to_session(Peer,Session)||Peer<-State#state.peers,Session<-Sessions],
    NewSessionMap=lists:foldl(fun(Item=#session_data{ssrc=SSRC},Dict)->dict:store(SSRC,Item,Dict) end, State#state.sessions),
    Reply={ok,#{track_map=>Sessions}},
    {reply,Reply,State#state{peers = dict:store(PeerId, NewPeer, State#state.peers),sessions = NewSessionMap}};

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
        
handle_cast({remove_track,#{peer_id:=PeerId,ssrc:=SSRC}},State)->
    {noreply,State};        

handle_cast({update_track,#{peer_id:=PeerId,ssrc:=SSRC,track:=Track}},State)->
    {noreply,State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

compute_peer_data(ConnectParams,_State)->
    #{  peer_pid:=PeerPid,
        peer_id:=PeerId,
        rtp_params:=#rtp_params{
            candidates=Candidates,
            tracks=Tracks}}
    =ConnectParams,
    #peer_data{id = PeerId,pid = PeerPid, candidates=Candidates, tracks = Tracks}.

inner_update_candidates(Candidates,PeerData)->
    PeerData#peer_data{candidates = Candidates}.


generate_rtp_sessions(_=#peer_data{tracks = Tracks})->
    Sessions=[generate_rtp_session(T)||T<-Tracks],
    {ok,Sessions}.

generate_rtp_session(InputSessionData=#{peer_id := PeerId,track := Track=#track{},constraints:=Constraints})->
    SSRC=generate_ssrc(),
    {ok,RTPSessionPid}=rtp_session:start(#{source_peer_id=>PeerId,ssrc=>SSRC,track=>Track,source_constraints=>Constraints}),
    InputSessionData=#session_data{ssrc=SSRC, source_peer_id = PeerId, trackname = Track#track.id,rtp_session_pid = RTPSessionPid},
    InputSessionData.

generate_ssrc()->
    rand:uniform().

subscribe_peer_to_session(_=#peer_data{pid = Pid},_=#session_data{rtp_session_pid = RtpSessionPid})->
    rtp_session:add_subscriber(RtpSessionPid,#{peer_id=>Pid}).



