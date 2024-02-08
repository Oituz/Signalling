-module(signalling_sfu_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([create_connection/2]).

%-------------------------------- Callback API -----------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%----------------------------------------------------------------------------------------


-define(NAME, ?MODULE).
-record(state, {sfumap}).

%---------------------------------API------------------------------------------------------


-spec create_connection(CallerPeerId::integer()|string(),MeetingId::integer()|string())->{ok,ConnectionId::pid()} | {error,Reason::any()}.
create_connection(CallerPeerId,MeetingId)->
    gen_server:call(?NAME,{create_connection,CallerPeerId,MeetingId}).
    
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{sfumap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


    
 handle_call({create_connection,CallerPeerId,SFUId},From,State)->
    case dict:find(CallerPeerId, State#state.sfumap) of
        {ok,PeerPid} -> ok=signalling_peer:create_connection(PeerPid,SFUId,From),
                        {noreply,State};
         error -> {ok,WorkerPid}=signalling_peer:start(CallerPeerId),
                  NewDict=dict:store(CallerPeerId, WorkerPid, State#state.sfumap),
                  ok=signalling_peer:create_connection(WorkerPid,SFUId,From),
                  {noreply,State#state{sfumap = NewDict}}
    end;   

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


