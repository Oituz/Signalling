
-export_type([
        connect_params/0,
        connect_response/0,
        add_subscriber_params/0,
        rtp_session_start_params/0,
        update_track_params/0,
        add_track_params/0,
        update_candidates_params/0]).
-include("../include/rtp.hrl").

-type add_subscriber_params()::#add_subscriber_params{
    peer_id::any(),
    peer_pid::pid()
}.

-record(add_subscriber_params,{
    peer_id::any(),
    peer_pid::pid()
}).

-type rtp_session_start_params()::#rtp_session_start_params{
    source_peer_id::pid(),
    ssrc::integer(),
    track::rtp:track()
}.
-record(rtp_session_start_params,{
    source_peer_id::pid(),
    ssrc::integer(),
    track::rtp:track()
}).

-type connect_params()::#connect_params{
    peer_id::integer() | string() | binary(),
    rtp_params::rtp:rtp_params()
}.

-type connect_response()::#connect_response{
    ssrc_session_map::map()
}.
-record(connect_response,{
    ssrc_session_map::map()
}).
-record(connect_params,{
    peer_id::integer(),
    rtp_params::rtp:rtp_params()
}).

-type update_candidates_params()::#update_candidates_params{
    peer_id::peer_id(),
    candidates::[rtp:candidate()]
}.
-record(update_candidates_params,{
    peer_id::peer_id(),
    candidates::[rtp:candidate()]
}).

-type update_track_params()::#update_track_params{
    ssrc::rtp:ssrc(),
    track::rtp:track()
}.

-record(update_track_params,{
    ssrc::rtp:ssrc(),
    track::rtp:track()
}).

-type add_track_params()::#add_track_params{
    peer_id::rtp:peer_id(),
    track::rtp:track()
}.

-record(add_track_params,{
    peer_id::rtp:peer_id(),
    track::rtp:track()
}).
