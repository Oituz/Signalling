%%%-------------------------------------------------------------------
%% @doc signalling public API
%% @end
%%%-------------------------------------------------------------------


-module(signalling_app).
-import(rtp, [wrtc_args/0,rtp_connection/0]).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("sugi pwla"),
    signalling_sup:start_link().


-spec create_connection(Args::rtp:wrtc_args())-> {ok,Connection::rtp:rtp_connection()} | {error,Reason::any()}.
create_connection(Args)->
    Connection=signalling_server:create_connection(Args),
    Connection.

stop(_State) ->
    ok.

%% internal functions
