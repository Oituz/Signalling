-module(signalling_worker).
-behaviour(gen_server).

%% API
-export([start_link/1,start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    id,
    stms
}).

-record(stm,{
    pid
}).
start(Args)->
    #{id := Id}=Args,
    signalling_worker_sup:start_worker(Id).
start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).

create_connection(Pid,PeerId)->
    gen_server:call(Pid,{initiate_negotiation, PeerId}).


init(Id) ->
    {ok, #state{id=Id,stms=dict:new()}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({initiate_negotiation,PeerId},From,State=#state{stms=StmMap})->
    case dict:find(PeerId,StmMap) of
        {ok,#stm{pid=StmPid}} -> {ok,CurrentFsmState}=signalling_fsm:get_state(StmPid),
                                 {reply,{already_negotiating,{state,CurrentFsmState}},State};
        error ->
            
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
