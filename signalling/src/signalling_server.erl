-module(signalling_server).
-behaviour(gen_server).

-import(rtp,[rtp_connection/0,wrtc_args/0]).
%% API
-export([start_link/0]).
-export([create_connection/1]).

%-------------------------------- Callback API -----------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%----------------------------------------------------------------------------------------


-define(NAME, ?MODULE).
-record(state, {peerMap=undefined}).


%---------------------------------API------------------------------------------------------

-spec create_connection(Args::rtp:wrtc_args())->{ok,Connection::rtp:rtp_connection()} | {error,Reason::any()}.
create_connection(Args)->
    gen_server:call(?NAME,Args).
    
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{peerMap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

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


