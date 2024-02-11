-module(signalling_sfu).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1,connect/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {id,peers}).

-record(peer_data,{
    id,
    pid,
    candidates
}).
-spec connect(SfuPid::pid(),PeerData::#{peer_id=>integer()|string()|binary(),candidates=>list()})->ok | {error,Reason::any()}.
connect(Pid,PeerData)->
    gen_server:call(Pid,{connect,PeerData}).
start(Args)->
    signalling_sfu_sup:start(Args).
stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(Id) ->
    {ok, #state{id=Id, peers=dict:new()}}.

handle_call({connect,#{peer_id:=PeerId,candidates:=Candidates}},From,State)->
    PeerData=#peer_data{candidates = Candidates,id=PeerId,pid = From},
    {reply,ok,State#state{peers = dict:store(PeerId, PeerData, State#state.peers)}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
