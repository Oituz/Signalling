-module(signalling_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([create_connection/2]).

%-------------------------------- Callback API -----------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%----------------------------------------------------------------------------------------


-define(NAME, ?MODULE).
-record(state, {peerMap}).

%---------------------------------API------------------------------------------------------


-spec create_connection(CallerPeerId::integer()|string(),APeerId::integer()|string())->{ok,ConnectionId::pid()} | {error,Reason::any()}.
create_connection(CallerPeerId,APeerId)->
    gen_server:call(?NAME,{create_connection,CallerPeerId,APeerId}).
    
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{peerMap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


    
 handle_call({create_connection,CallerPeerId,APeerId},From,State)->
    case dict:find(CallerPeerId, State#state.peerMap) of
        {ok,PeerPid} -> ok=signalling_worker:create_connection(PeerPid,APeerId,From),
                        {noreply,State};
         error -> {ok,WorkerPid}=signalling_worker:start(CallerPeerId),
                  NewDict=dict:store(CallerPeerId, WorkerPid, State#state.peerMap),
                  ok=signalling_worker:create_connection(WorkerPid,APeerId,From),
                  {noreply,State#state{peerMap = NewDict}}
    end;   

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


