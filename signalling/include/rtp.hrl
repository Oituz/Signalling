
-export_type([track/0,media_constraint/0,ice_candidate/0,track/0,rtp_params/0]).

-record(rtp_params,{
        candidates::[ice_candidate()],
        tracks::[track()],
        constraints::[media_constraint()]
}).
-record(ice_candidate,{
        transport_protocol :: string(),
        ip_address::string(),
        port ::integer(),
        attributes::string()
}).
-type rtp_params()::#rtp_params{
     candidates::[ice_candidate()],
     constraints::[media_constraint()],
     tracks::[track()]
}.
-type ice_candidate()::#ice_candidate{
        transport_protocol :: string(),
        ip_address::string(),
        port ::integer(),
        attributes::string()  
}.
-record(media_constraint,{
   type::string()
}).
-type media_constraint()::#media_constraint{
    type::string()
}.

-record(track,{
        type::string()
}).

-type track()::#track{
        type::string()
}.




