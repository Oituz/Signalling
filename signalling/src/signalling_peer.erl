-module(signalling_peer).
-behaviour(gen_server).

%% API
-export([start_link/1,start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,create_connection/2]).
-record(state, {
    id,
    sfu_pid
}).

-record(stm,{
    fsm_pid,
    notify_pid
}).
start(Args)->
    signalling_peer_sup:start_worker(Args).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).
-spec create_connection(PeerId::integer()|string(),NotifyPid::pid())->{ok,PeerPid::pid()}.
create_connection(PeerId,MeetingId)->
    {ok,PeerPid}=signalling_peer:start(#{id => PeerId}),
    gen_server:call(PeerPid,{create_connection,MeetingId}).

init(Args) ->
    #{id :=Id}=Args,
    {ok, #state{id=Id}}.



handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({create_connection,MeetingId},_From,State=#state{id=PeerId})->
      {ok,SfuPid}=signalling_sfu_server:create_connection(PeerId,MeetingId),
      {reply,{ok,SfuPid},State};

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
