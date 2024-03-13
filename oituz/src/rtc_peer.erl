-module(rtc_peer).
-behaviour(gen_server).
-include("../include/rtp.hrl").
-include("../include/domain.hrl").

%% API
-export([start_link/1,join_meeting/3,publish_stream_data/2,broadcast_stream_data/2]).

-export([update_candidates/2,update_track/3,add_track/2,remove_track/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    id,
    sfu_pid=undefined
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------- API -------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(PeerData) ->
    gen_server:start_link(?MODULE,PeerData,[]).

-spec join_meeting(PeerId::peer_id(),MeetingId::meeting_id(),RTPParams::rtp:rtp_params())->{ok,PeerPid::pid()} | {error,Reason::any()}.
join_meeting(PeerId,MeetingId,RTPParams)->
    {ok,PeerPid}=register_service:get_peer(PeerId),
    gen_server:call(PeerPid,{caller_message,{join_meeting,#{meeting_id=>MeetingId,rtp_params=>RTPParams}}}).

-spec update_candidates(PeerPid::pid(),Candidates::[rtp:ice_candidate()])->ok.
update_candidates(PeerPid,Candidates)->
    gen_server:cast(PeerPid,{caller_message,{update_candidates,Candidates}}).

-spec update_track(PeerPid::pid(),SSRC::ssrc(),Track::rtp:track())->ok.
update_track(PeerPid,SSRC,Track)->
    gen_server:cast(PeerPid,{caller_message,{update_track,SSRC,Track}}).

-spec add_track(PeerPid::pid(),Track::rtp:track())->ok.
add_track(PeerPid,Track)->
    gen_server:cast(PeerPid,{caller_message,{add_track,Track}}).

-spec remove_track(PeerPid::pid(),SSRC::ssrc())->ok.
remove_track(PeerPid,SSRC)->
    gen_server:cast(PeerPid,{caller_message,{remove_track,SSRC}}).
-spec publish_stream_data(PeerPid::pid(),StreamData::binary())->ok.
publish_stream_data(PeerPid,StreamData)->
    gen_server:cast(PeerPid,{caller_message,{publish_stream_data,StreamData}}).

-spec broadcast_stream_data(PeerPid::pid(),StreamData::binary())->ok.
broadcast_stream_data(PeerPid,StreamData)->
    gen_server:cast(PeerPid,{rtp_message,{broadcast_stream_data,StreamData}}).
init(_=#{id:=Id}) ->
    {ok, #state{id=Id}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------------------- Callbacks ---------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({caller_message,{stream_data,StreamData}},State) when erlang:is_binary(StreamData)->
    {noreply,State};


handle_cast({caller_message,{update_candidates,Candidates}},State=#{id:=Id,sfu_pid := SfuPid})->
    Message=#update_candidates_params{peer_id=Id,candidates=Candidates},
    ok=sfu:update_candidates(SfuPid,Message),
    {noreply,State};

handle_cast({caller_message,{add_track,Track}},_=#state{id = Id,sfu_pid = SfuPid})->
    Message=#add_track_params{peer_id = Id,track = Track},
    sfu:add_track(SfuPid,Message);
handle_cast({caller_message,{update_track,SSRC,Track}},State=#{sfu_pid := SfuPid})->
    Message=#update_track_params{ssrc=SSRC,track=Track},
    ok=sfu:update_track(SfuPid,Message),
    {noreply,State};

handle_cast({caller_message,{remove_track,SSRC}},_=#state{sfu_pid = SfuPid})->
    sfu:remove_track(SfuPid,SSRC);

handle_cast({rtp_message,{broadcast_stream_data,StreamData}},State) when erlang:is_binary(StreamData)->
    {noreply,State};


handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call({caller_message,{join_meeting,#{meeting_id:=MeetingId,rtp_params:=RTPParams}}},_From,State)->
    {ok,SfuPid}=register_service:get_sfu(MeetingId),
    ConnectData=#connect_params{peer_id = State#state.id, rtp_params=RTPParams},
    Result=sfu:connect(SfuPid,ConnectData),
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
