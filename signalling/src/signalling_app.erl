%%%-------------------------------------------------------------------
%% @doc signalling public API
%% @end
%%%-------------------------------------------------------------------


-module(signalling_app).
-import(rtp, [wrtc_args/0,rtp_connection/0]).
% -include("rtp.hrl").
-behaviour(application).

-export([start/2, stop/1,create_connection/2]).

start(_StartType, _StartArgs) ->
    io:format("sugi pwla"),
    mnesia:start(),
    signalling_sup:start_link().


% -spec create_connection(Args::rtp:wrtc_args())-> {ok,Connection::rtp:rtp_connection()} | {error,Reason::any()}.

% create_connection(Args=#rtp:wrtc_args{self_id=Id, connect_with_id=ConnectWithId})->
%     ProxyPid=signalling_server:create_connection(Id,ConnectWithId),
%     Connection.
-spec create_connection(PeerId::integer(),MeetingId::integer())->{ok,PeerProcess::pid()}|{error,Reason::any()}.
create_connection(PeerId,MeetingId)->
    Result=signalling_peer:create_connection(PeerId,MeetingId),
    Result.
stop(_State) ->
    ok.

%% internal functions
