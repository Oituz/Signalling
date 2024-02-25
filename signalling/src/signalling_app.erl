%%%-------------------------------------------------------------------
%% @doc signalling public API
%% @end
%%%-------------------------------------------------------------------


-module(signalling_app).
-import(rtp, [wrtc_args/0,rtp_connection/0]).
-include("../include/rtp.hrl").
-behaviour(application).
-export([create_connection/3,
        update_candidates/2,
        update_tracks/2,
        update_media_constraints/2,
        cr/0]).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    
    mnesia:start(),
    signalling_sup:start_link().

-spec create_connection(PeerId::integer(),MeetingId::integer(),RTPParams::rtp:rtp_params())->{ok,PeerProcess::pid()}|{error,Reason::any()}.
create_connection(PeerId,MeetingId,RTPParams)->
    Result=signalling_peer:join_meeting(PeerId,MeetingId,RTPParams),
    Result.

cr()->
    Result=signalling_peer:join_meeting(11,11,#rtp_params{}),
    Result.


-spec update_candidates(PeerPid::pid(),Candidates::[rtp:ice_candidate()])->ok.
update_candidates(PeerPid,Candidates)->
    signalling_peer:update_candidates(PeerPid,Candidates).

-spec update_tracks(PeerPid::pid(),Tracks::[rtp:track()])->ok.
update_tracks(PeerPid,Tracks)->
    signalling_peer:update_tracks(PeerPid,Tracks).

-spec update_media_constraints(PeerPid::pid(),Constraints::[rtp:constraint()])->ok.
update_media_constraints(PeerPid,Constraints)->
    signalling_peer:update_constraints(PeerPid,Constraints).
stop(_State) ->
    ok.

%% internal functions
