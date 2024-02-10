-module(register_service).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([get_sfu/1]).

%-------------------------------- Callback API -----------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%----------------------------------------------------------------------------------------


-define(NAME, ?MODULE).
-record(state, {sfumap}).

%---------------------------------API------------------------------------------------------


-spec get_sfu(MeetingId::integer()|string())->{ok,SfuPid::pid()} | {error,Reason::any()}.
get_sfu(MeetingId)->
    gen_server:call(?NAME,{get_sfu,MeetingId}).
    
-spec get_peer(PeerId::integer()|string())->{ok,PeerPid::pid()} | {error,Reason::any()}.
get_peer(PeerId)->
    gen_server:call(?NAME,{get_sfu,PeerId}).
    
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{sfumap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


    
 handle_call({get_sfu,SFUId},_From,State)->
     case cache:lookup_sfu(SFUId) of
        not_found -> {ok,SfuPid}=signalling_sfu_sup:start(#{id=>SFUId}),
                     ok=cache:update_sfu(SFUId,SfuPid),
                     {reply,{ok,SfuPid},State};
        {ok,SfuPid} -> {reply,{ok,SfuPid},State}
     end;

handle_call({get_peer,PeerId},_From,State)->
    case cache:lookup(PeerId) of
        not_found -> {ok,PeerPid}=signalling_peer_sup:start(#{id=>PeerId}),
                      ok=cache:update_peer(PeerId,PeerPid),
                      {reply,{ok,PeerPid},State};
        {ok,PeerPid} -> {reply,{ok,PeerPid},State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


