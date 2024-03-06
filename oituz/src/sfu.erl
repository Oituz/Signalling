-module(sfu).
-behaviour(gen_server).
-include("../include/rtp.hrl").
%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([update_candidates/2,update_tracks/2]).
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
    session_pid,
    originating_peer_id
}).
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

-spec update_tracks(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),tracks=>[rtp:track()]})->ok.
update_tracks(SfuPid,_PeerData=#{peer_id:=PeerId,tracks:=Tracks})->
    gen_server:cast(SfuPid,{update_tracks,#{peer_id=>PeerId,tracks=>Tracks}}).
start(Args)->
    sfu_sup:start(Args).
stop(Name) ->
    gen_server:call(Name, stop).

start_link(SfuData) ->
    gen_server:start_link(?MODULE, SfuData, []).

init(_=#{id:=Id}) ->
    {ok, #state{id=Id, peers=dict:new()},sessions=dict:new()}.

handle_call({connect,ConnectParams=#{peer_id:=PeerId}},_From,State)->
    PeerData=compute_peer_data(ConnectParams,State),
    Sessions=generate_sessions(PeerData),
    NewSessionMap=lists:foldl(fun(Item=#session_data{ssrc :=SSRC},Dict)->dict:store(SSRC,Item,Dict) end, State#state.sessions),
    Reply={ok,#{track_map=>Sessions}},
    {reply,Reply,State#state{peers = dict:store(PeerId, PeerData, State#state.peers),sessions = NewSessionMap}};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_candidates,#{peer_id:=PeerId,candidates:=Candidates}},State)->
    case dict:find(PeerId, State#state.peers) of
        error -> io:format("Could not find peer ~p",[PeerId]),
                 {noreply,State};`
                 
        {ok,PeerData}-> NewPeerData=inner_update_candidates(Candidates, PeerData),
                        NewPeers=dict:store(PeerId, NewPeerData,State#state.peers),
                        {noreply,State#state{peers = NewPeers}}
    end;
   

handle_cast({update_tracks,#{peer_id:=PeerId,tracks:=Tracks}},State)->
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

generate_sessions(_=#peer_data{tracks = Tracks})->
    Sessions=[generate_session(T)||T<-Tracks],
    {ok,Sessions}.

generate_session(SessionData=#{name :=Name , peer_id := PeerId,track := Track=#track{},constraints:=Constraints})->
    SSRC=generate_ssrc(),
    {ok,SessionPid}=rtp_session:start(#{sourcing_peer_id=>PeerId,ssrc=>SSRC,track=>Track,source_constraints=>Constraints}),
    #{pid=>SessionPid,ssrc=>SSRC,track=>Track}.

generate_ssrc()->
    rand:uniform().


