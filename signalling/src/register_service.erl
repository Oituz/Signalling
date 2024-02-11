-module(register_service).
-behaviour(gen_server).
%% API
-export([start_link/0]).
-export([get_sfu/1,get_peer/1,remove_peer/1,remove_sfu/1]).

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
    gen_server:call(?NAME,{get_peer,PeerId}).

remove_peer(PeerId)->
    gen_server:call(?NAME,{remove_peer,PeerId}).

remove_sfu(SFUId)->
    gen_server:call(?NAME,{remove_sfu,SFUId}).
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{sfumap=dict:new()}}.


%---------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};


    
 handle_call({get_sfu,SFUId},_From,State)->
     case cache:lookup_sfu(SFUId) of
        not_found->{ok,SfuPid}=signalling_sfu_sup:start(#{id=>SFUId}),
                               Data=#{sfu_pid=>SfuPid,sup_pid=>whereis(signalling_sfu_sup)},
                               ok=cache:update_sfu(SFUId,Data),
                               {reply,{ok,SfuPid},State};
        {ok,SfuPid}-> {reply,{ok,SfuPid},State}
     end;

handle_call({get_peer,PeerId},_From,State)->
    case cache:lookup_peer(PeerId) of
        not_found -> {ok,PeerPid}=signalling_peer_sup:start(#{id=>PeerId}),
                     Data=#{sfu_pid=>PeerPid,sup_pid=>whereis(signalling_peer_sup)},
                     ok=cache:update_peer(PeerId,Data),
                     {reply,{ok,PeerPid},State};
        {ok,PeerPid} -> {reply,{ok,PeerPid},State}
    end;

handle_call({remove_peer,PeerId},_From,State)->
    case cache:lookup_peer(PeerId) of
        {ok,#{peer_pid := PeerPid , sup_pid := SupPid}} -> Result=signalling_peer_sup:remove(SupPid,PeerPid),
                                                           {reply,Result,State};
        not_found -> {reply,ok,State}
    end;

handle_call({remove_sfu,SfuId},_From,State)->
    case cache:lookup_sfu(SfuId) of
        {ok,#{sfu_pid :=SFUPid  , sup_pid := SupPid}} -> Result=signalling_sfu_sup:remove(SupPid,SFUPid),
                                                           {reply,Result,State};
        not_found -> {reply,ok,State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


