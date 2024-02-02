-module(signalling_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([create_connection/1]).

%-------------------------------- Callback API -----------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%----------------------------------------------------------------------------------------


-define(NAME, ?MODULE).
-record(state, {peerMap=undefined}).

%---------------------------------API------------------------------------------------------


-spec create_connection(Id::integer()|string(),WithId::integer()|string())->{ok,ConnectionId::pid()} | {error,Reason::any()}.
create_connection(Id,WithId)->
    gen_server:call(?NAME,{create_connection,Id,WithId}).
    
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{peerMap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({create_connection,Id,WithId}, _From, State)->
    {ok,WorkerPid}=signalling_worker:start(Id),
    NewDict=dict:store(Id, WorkerPid, State#state.peerMap),
    signalling_worker:create_connection(WorkerPid,WithId,_From),
    {noreply,State#state{peerMap = NewDict}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


