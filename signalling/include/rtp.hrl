% -module(rtp).
-export_type([rtp_connection/0,ice_candidate/0]).

-record(rtp_connection,{
            sdp_offer :: string(),
            sdp_answer ::string(),
            ice_candidates::[ice_candidate()]
}).
-record(ice_candidate,{
        transport_protocol :: string(),
        ip_address::string(),
        port ::integer(),
        attributes::string()
}).

-type rtp_connection() ::#rtp_connection{ice_candidates :: [ice_candidate()]}.
-type ice_candidate()::#ice_candidate{
        transport_protocol :: string(),
        ip_address::string(),
        port ::integer(),
        attributes::string()  
}.
-record(wrtc_args,{}).



