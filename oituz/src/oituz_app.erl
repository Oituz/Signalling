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
        add_track/2,
        remove_track/2,
        update_track/3
        ]).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    oituz_sup:start_link().

-spec connect(PeerId::peer_id(),MeetingId::meeting_id(),RTPParams::rtp:rtp_params())->{ok,PeerProcess::pid()}|{error,Reason::any()}.
connect(PeerId,MeetingId,RTPParams)->
    case rtc_peer:join_meeting(PeerId,MeetingId,RTPParams) of
        {ok,PeerPid} -> {ok,PeerPid};
        {error,Reason} -> {error,Reason}
    end.


-spec update_candidates(PeerPid::pid(),Candidates::[rtp:ice_candidate()])->ok.
update_candidates(PeerPid,Candidates)->
    rtc_peer:update_candidates(PeerPid,Candidates).

-spec update_track(PeerPid::pid(),SSRC::ssrc(),Track::rtp:track())->ok.
update_track(PeerPid,SSRC,Track)->
    rtc_peer:update_track(PeerPid,SSRC,Track).

-spec add_track(PeerPid::pid(),Track::rtp:track())->ok.
add_track(PeerPid,Track)->
    rtc_peer:add_track(PeerPid,Track).

-spec remove_track(PeerPid::pid(),SSRC::ssrc())->ok.

remove_track(PeerPid,SSRC)->
    rtc_peer:remove_track(PeerPid,SSRC).
stop(_State) ->
    ok.

%% internal functions
