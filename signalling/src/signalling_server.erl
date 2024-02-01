-module(signalling_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([create_proxy/1]).

%-------------------------------- Callback API -----------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%----------------------------------------------------------------------------------------


-define(NAME, ?MODULE).
-record(state, {proxyMap=undefined}).

%---------------------------------API------------------------------------------------------


-spec create_proxy(Id::integer()|string()|binary())->{ok,ProxyId::pid()} | {error,Reason::any()}.
create_proxy(Id)->
    gen_server:call(?NAME,{create_proxy,Id}).
    
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{proxyMap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({create_proxy,Id}, _From, State)->
    {ok,Pid}=signalling_worker:start(Id),
    NewDict=dict:store(Id, Pid, State#state.proxyMap),
    {reply,{ok,Pid},State#state{proxyMap = NewDict}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


