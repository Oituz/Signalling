%%%-------------------------------------------------------------------
%% @doc signalling public API
%% @end
%%%-------------------------------------------------------------------


-module(signalling_app).
-import(rtp, []).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("sugi pwla"),
    signalling_sup:start_link().


-spec create_connection(PeerId::wrt())->
stop(_State) ->
    ok.

%% internal functions
