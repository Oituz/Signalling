-module(rtc_peer).
-behaviour(gen_server).

%% API
-export([start_link/1,join_meeting/3,publish_stream_data/2,broadcast_stream_data/2]).
-export([update_candidates/2,update_constraints/2,update_tracks/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    id,
    sfu_pid=undefined
}).


start_link(PeerData) ->
    gen_server:start_link(?MODULE,PeerData,[]).

-spec join_meeting(PeerId::integer()|string(),MeetingId::integer()|string(),RTPParams::rtp:rtp_params())->{ok,PeerPid::pid()} | {error,Reason::any()}.
join_meeting(PeerId,MeetingId,RTPParams)->
    {ok,PeerPid}=register_service:get_peer(PeerId),
    gen_server:call(PeerPid,{caller_message,{join_meeting,#{meeting_id=>MeetingId,rtp_params=>RTPParams}}}).

-spec update_candidates(PeerPid::pid(),Candidates::[rtp:ice_candidate()])->ok.
update_candidates(PeerPid,Candidates)->
    gen_server:cast(PeerPid,{caller_message,{update_candidates,Candidates}}).

-spec update_tracks(PeerPid::pid(),Tracks::[rtp:track()])->ok.
update_tracks(PeerPid,Tracks)->
    gen_server:cast(PeerPid,{caller_message,{update_tracks,Tracks}}).

-spec update_constraints(PeerPid::pid(),Constraints::[rtp:constraint()])->ok.
update_constraints(PeerPid,Constraints)->
    gen_server:cast(PeerPid,{caller_message,{update_constraints,Constraints}}).
-spec publish_stream_data(PeerPid::pid(),StreamData::binary())->ok.
publish_stream_data(PeerPid,StreamData)->
    gen_server:cast(PeerPid,{caller_message,{publish_stream_data,StreamData}}).

-spec broadcast_stream_data(PeerPid::pid(),StreamData::binary())->ok.
broadcast_stream_data(PeerPid,StreamData)->
    gen_server:cast(PeerPid,{sfu_message,{broadcast_stream_data,StreamData}}).
init(_=#{id:=Id}) ->
    {ok, #state{id=Id}}.

handle_cast({stream_data,StreamData},State) when erlang:is_binary(StreamData)->
    {noreply,State};


handle_cast({caller_message,{update_candidates,Candidates}},State=#{id:=Id,sfu_pid := SfuPid})->
    Message=#{peer_id=>Id,candidates=>Candidates},
    ok=sfu:update_candidates(SfuPid,Message),
    {noreply,State};

handle_cast({caller_message,{update_tracks,Tracks}},State=#{id:=Id,sfu_pid := SfuPid})->
    Message=#{peer_id=>Id,tracks=>Tracks},
    ok=sfu:update_tracks(SfuPid,Message),
    {noreply,State};

handle_cast({caller_message,{update_constraints,Constraints}},State=#{id:=Id,sfu_pid := SfuPid})->
    Message=#{peer_id=>Id,constraints=>Constraints},
    ok=sfu:update_constraints(SfuPid,Message),
    {noreply,State};

handle_cast({sfu_message,{broadcast_stream_data,StreamData}},State)->
    {noreply,State};


handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call({caller_message,{join_meeting,#{meeting_id:=MeetingId,rtp_params:=RTPParams}}},_From,State)->
    {ok,SfuPid}=register_service:get_sfu(MeetingId),
    ConnectData=#{peerId =>State#state.id,peer_pid=>self(),rtp_params=>RTPParams},
    Result=case sfu:connect(SfuPid,ConnectData) of
                ok -> {ok,self()};
                {error,Reason} -> {error,Reason}
            end,
    {reply,Result,State#state{sfu_pid = SfuPid}};
   

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