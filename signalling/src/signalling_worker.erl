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
    fsm_pid,
    notify_pid
}).
start(Args)->
    #{id := Id}=Args,
    signalling_worker_sup:start_worker(Id).
start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [], []).
-spec create_connection(Pid::pid(),PeerId::integer()|string(),NotifyPid::pid())->ok.
create_connection(Pid,PeerId,NotifyPid)->
    gen_server:cast(Pid,{initiate_negotiation, PeerId,NotifyPid}).

send_signalling_message(Pid,PeerId,Message)->
    gen_server:cast(Pid, {signalling_message,PeerId,Message}).

init(Id) ->
    {ok, #state{id=Id,stms=dict:new()}}.


handle_cast({initiate_negotiation,PeerId,NotifyPid},State=#state{stms=StmMap,id=Id})->
    case dict:find(PeerId,StmMap) of
        {ok,#stm{fsm_pid=StmPid}} -> {ok,CurrentFsmState}=signalling_fsm:get_state(StmPid),
                                     gen_server:reply(NotifyPid, {already_negotiating,{state,CurrentFsmState}}),
                                     {noreply,State};
        error -> {ok,Pid}=signalling_fsm:start({new_fsm,Id,PeerId,NotifyPid}),
                 NewDict=dict:store(PeerId, #stm{fsm_pid=Pid,notify_pid=NotifyPid},StmMap),
                 {noreply,State#state{stms=NewDict}}
    end;
handle_cast({signalling_message,PeerId,Message},State=#state{stms=StmMap,id=Id})->
    case dict:find(PeerId,StmMap) of
        error -> io:format("Could not find peer with id")
                {noreply,State};
        {ok,#stm{fsm_pid=StmPid}}-> ok=signalling_fsm:send_signalling_message(StmPid,Message),



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
