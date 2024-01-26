-module(signalling_server).
-behaviour(gen_server).
-define(Name, ?MODULE).
%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-record(state, {peerMap=undefined}).

-spec initiate_session(Pid::pid(),PeerId::binary())->pid().
initiate_session(Pid,PeerId)->
    gen_server:call(Pid, {initiate_session,PeerId}).
start_link() ->
    gen_server:start_link({local, ?Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{peerMap=dict:new()}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({initiate_session,PeerId}, _From, State)->
    {ok,Pid}=signalling_worker:start(PeerId),
    NewDict=dict:store(PeerId, Pid, State#state.peerMap),
    {reply,{ok,Pid},State#state{peerMap = NewDict}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


