-module(rtp).
-export_type([wrtc_args/0, ice_candidate/0, rtp_connection/0]).


-type ice_candidate()::{
    Transport_Protocol :: string(),
    IP_Address::string(),
    Port ::integer(),
    Attributes::string()
}.

-type rtp_connection() :: {
        Sdp_Offer :: string(),
        Sdp_Answer ::string(),
        Ice_Candidates::[ice_candidate()]
}.
-type wrtc_args() :: {}.



