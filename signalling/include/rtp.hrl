% -module(rtp).
-export_type([rtp_connection/0,ice_candidate/0,wrtc_args/0]).

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
-type wrtc_args()::#wrtc_args{ self_id::integer()|string()|binary() , connect_with_id::integer()|string()|binary()}.
-record(wrtc_args,{ self_id,connect_with_id}).



