-module(signalling_sfu).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([update_candidates/2,update_tracks/2,update_constraints/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {id,peers}).

-record(peer_data,{
    id,
    pid,
    candidates,
    tracks
}).
-spec connect(SfuPid::pid(),ConnectData)->ok | {error,Reason::any()} 
    when ConnectData::#{
        candidates=>[rtp:ice_candidate()],
        
    }.
connect(Pid,ConnectData)->
    gen_server:call(Pid,{connect,ConnectData}).

-spec update_candidates(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),candidates=>Candidates::list()})->ok.
update_candidates(SfuPid,PeerData=#{peer_id:=PeerId,candidates:=Candidates})->
    gen_server:cast(SfuPid,{update_candidates,#{peer_id=>PeerId,candidates=>Candidates}}).

-spec update_constraints(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary()})->ok.
update_constraints(SfuPid,PeerData=#{peer_id:=PeerId,constraints:=Constraints})->
    gen_server:cast(SfuPid,{update_constraints,#{peer_id=>PeerId,constraints=>Constraints}}).

-spec update_tracks(SfuPid::pid(),PeerData::#{peer_id=>PeerId::integer()|string()|binary(),tracks=>Tracks::list()})->ok.
update_tracks(SfuPid,PeerData=#{peer_id:=PeerId,tracks:=Tracks})->
    gen_server:cast(SfuPid,{update_tracks,#{peer_id=>PeerId,tracks=>Tracks}}).
start(Args)->
    signalling_sfu_sup:start(Args).
stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(Id) ->
    {ok, #state{id=Id, peers=dict:new()}}.

handle_call({connect,#{peer_id:=PeerId,PeerData}},From,State)->
    PeerData=#peer_data{candidates = Candidates,id=PeerId,pid = From,tracks=Tracks},
    {reply,ok,State#state{peers = dict:store(PeerId, PeerData, State#state.peers)}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_candidates,#{peer_id:=PeerId,candidates:=Candidates}},State)->
    {noreply,State};
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

create_peer_data(PeerData=#{})