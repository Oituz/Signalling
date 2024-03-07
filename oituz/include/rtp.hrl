
-export_type([track/0,media_constraint/0,ice_candidate/0,track/0,rtp_params/0]).


-record(ice_candidate,{
        transport_protocol :: string(),
        ip_address::string(),
        port ::integer(),
        attributes::string()
}).

-type ice_candidate()::#ice_candidate{
        transport_protocol :: string(),
        ip_address::string(),
        port ::integer(),
        attributes::string()  
}.


-record(rtp_params,{
        candidates::[ice_candidate()],
        tracks::[track()]
}).

-type rtp_params()::#rtp_params{
     candidates::[ice_candidate()],
     tracks::[track()]
}.

-record(media_constraint,{
   type::string(),
   value::any()
}).
-type media_constraint()::#media_constraint{
    type::string(),
    value::any()
}.

-record(track,{
        id::string(),
        type::string(),
        constraints::[media_constraint()]
}).

-type track()::#track{
        id::string(),
        type::string(),
        constraints::[media_constraint()]
}.




