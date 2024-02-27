-module(sfu).
-behaviour(gen_server).
-include("../include/rtp.hrl").
%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([update_candidates/2,update_tracks/2,update_constraints/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {id,peers}).

-record(peer_data,{
    id,
    pid,
    candidates,
    tracks,
    constraints
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

-spec update_constraints(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),constraints=>[rtp:constraint()]})->ok.
update_constraints(SfuPid,_PeerData=#{peer_id:=PeerId,constraints:=Constraints})->
    gen_server:cast(SfuPid,{update_constraints,#{peer_id=>PeerId,constraints=>Constraints}}).

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
    {ok, #state{id=Id, peers=dict:new()}}.

handle_call({connect,ConnectParams=#{peer_id:=PeerId}},_From,State)->
    PeerData=compute_peer_data(ConnectParams,State),
    {reply,ok,State#state{peers = dict:store(PeerId, PeerData, State#state.peers)}};
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
   
handle_cast({update_constraints,#{peer_id:=PeerId,constraints:=Constraints}},State)->
    {noreply,State}; 
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
            constraints=Constraints,
            tracks=Tracks}}
    =ConnectParams,
    #peer_data{id = PeerId,pid = PeerPid, candidates=Candidates, tracks = Tracks, constraints = Constraints}.

inner_update_candidates(Candidates,PeerData)->
    PeerData#peer_data{candidates = Candidates}.


