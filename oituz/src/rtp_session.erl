-module(rtp_session).
-behaviour(gen_server).

%% API
-export([
        add_subscriber/2,
        get_subscribers/1,
        remove_subscriber/2,
        broadcast_message/2]).
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    ssrc,
    subscribers
    }).


-spec broadcast_message(SessionPid::pid(),Message::any())->ok.
broadcast_message(SessionPid,Message)->
    gen_server:cast(SessionPid,{broadcast_message,Message}).
-spec add_subscriber(SessionPid::pid(),Subscriber::map())->ok | {error,Error::any()}.
add_subscriber(SessionPid,Subscriber)->
    gen_server:call(SessionPid,{add_subscriber,Subscriber}).

-spec remove_subscriber(SessionPid::pid(),SubscriberId::integer())->{ok | {error,Error::any()}}.
remove_subscriber(SessionPid,SubscriberId)->
    gen_server:call(SessionPid,{remove_subscriber,SubscriberId}).

-spec get_subscribers(SessionPid::pid())->{ok,Subscribers::list()} | {error,Error::any()}.
get_subscribers(SessionPid)->
    gen_server:call(SessionPid,get_subscribers).
start(RTPSessionData) ->
    rtp_session_sup:start_child(RTPSessionData).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(RTPSessionData) ->
    gen_server:start_link(?MODULE, RTPSessionData, []).

init(_=#{ssrc:=SSRC}) ->
    {ok, #state{subscribers = [],ssrc = SSRC}}.

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
