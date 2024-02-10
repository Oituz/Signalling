-module(signalling_sfu).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1,connect_to_sfu/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {id,peers}).

-spec connect_to_sfu(SfuPid::pid(),PeerData::#{Peer::pid(),Candidates::list()})->ok | {error,Reason::any()}.
connect_to_sfu(Pid,PeerData)->
    gen_server:call(Pid,{connect,PeerData}).
start(Args)->
    signalling_sfu_sup:start(Args).
stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    #{peers := Peers,id := Id}=_Args,
    {ok, #state{peers=Peers, id=Id}}.

handle_call({connect,Args},From,State)->

    {reply,ok,State};
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
