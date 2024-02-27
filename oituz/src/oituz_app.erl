%%%-------------------------------------------------------------------
%% @doc oituz public API
%% @end
%%%-------------------------------------------------------------------


-module(oituz_app).
-import(rtp, [wrtc_args/0,rtp_connection/0]).
-include("../include/rtp.hrl").
-behaviour(application).
-export([connect/3,
        update_candidates/2,
        update_tracks/2,
        update_media_constraints/2
        ]).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    oituz_sup:start_link().

-spec connect(PeerId::integer(),MeetingId::integer(),RTPParams::rtp:rtp_params())->{ok,PeerProcess::pid()}|{error,Reason::any()}.
connect(PeerId,MeetingId,RTPParams)->
    case rtc_peer:join_meeting(PeerId,MeetingId,RTPParams) of
        {ok,PeerPid} -> {ok,PeerPid};
        {error,Reason} -> {error,Reason}
    end.


-spec update_candidates(PeerPid::pid(),Candidates::[rtp:ice_candidate()])->ok.
update_candidates(PeerPid,Candidates)->
    rtc_peer:update_candidates(PeerPid,Candidates).

-spec update_tracks(PeerPid::pid(),Tracks::[rtp:track()])->ok.
update_tracks(PeerPid,Tracks)->
    rtc_peer:update_tracks(PeerPid,Tracks).

-spec update_media_constraints(PeerPid::pid(),Constraints::[rtp:constraint()])->ok.
update_media_constraints(PeerPid,Constraints)->
    rtc_peer:update_constraints(PeerPid,Constraints).
stop(_State) ->
    ok.

%% internal functions
