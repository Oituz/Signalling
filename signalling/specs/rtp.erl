-module(rtp).
-export([wrtc_args/0, ice_candidate/0, rtp_connection/0]).


%-------------------------

-type rtp_connection() :: {
        sdp_offer : string(),
        sdp_answer : string(),
        ice_candidates: [ice_candidate()]
}.
-type wrtc_args() :: {}.

-record(ice_candidate, {
    transport_protocol :: string(),
    ip_address :: string(),
    port :: integer(),
    attributes :: [string()]
}).

