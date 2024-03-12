
-export_type([
        connect_params/0,
        add_subscriber_params/0,
        rtp_session_start_params/0]).
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
-record(connect_params,{
    peer_id::integer(),
    rtp_params::rtp:rtp_params()
}).

