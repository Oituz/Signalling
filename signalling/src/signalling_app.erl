%%%-------------------------------------------------------------------
%% @doc signalling public API
%% @end
%%%-------------------------------------------------------------------


-module(signalling_app).
-import(rtp, [wrtc_args/0,rtp_connection/0]).
-include("rtp.hrl").
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("sugi pwla"),
    signalling_sup:start_link().


-spec create_connection(Args::rtp:wrtc_args())-> {ok,Connection::rtp:rtp_connection()} | {error,Reason::any()}.
create_connection(Args=rtp:wrtc_args{id=Id})->
    ProxyPid=signalling_server:create_proxy(Id),
    Connection.

stop(_State) ->
    ok.

%% internal functions
