-module(signalling_peer).
-behaviour(gen_server).

%% API
-export([start_link/1,join_meeting/3,stream_data/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    id,
    sfu_pid=undefined,
    candidates
}).


start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).

-spec join_meeting(PeerId::integer()|string(),MeetingId::integer()|string(),Candidates::[string()|binary()])->{ok,PeerPid::pid()} | {error,Reason::any()}.
join_meeting(PeerId,MeetingId,Candidates)->
    {ok,PeerPid}=register_service:get_peer(PeerId),
    gen_server:call(PeerPid,{caller_message,{join_meeting,#{meeting_id=>MeetingId,candidates=>Candidates}}}).

-spec stream_data(PeerPid::pid(),StreamData::binary())->ok.
stream_data(PeerPid,StreamData)->
    gen_server:cast(PeerPid,{stream_data,StreamData}).

init(Args) ->
    #{id :=Id}=Args,
    {ok, #state{id=Id}}.

handle_cast({stream_data,StreamData},State) when erlang:is_binary(StreamData)->
    
    {noreply,State};
handle_cast(_Msg, State) ->
    {noreply, State}.



handle_call({caller_message,{join_meeting,#{meeting_id:=MeetingId,candidates := Candidates,tracks:=Tracks}}},_From,State)->
    {ok,SfuPid}=register_service:get_sfu(MeetingId),
    ConnectData=#{peerId =>State#state.id,sfu_pid=>SfuPid,candidates=>Candidates,tracks=>Tracks},
    Result=signalling_sfu:connect(SfuPid,ConnectData),
    {reply,Result,State#state{sfu_pid = SfuPid,candidates = Candidates}};
   

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
