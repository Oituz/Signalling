-module(rtc_peer_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/rtp.hrl").
-compile([export_all]).



all()->[can_join_meeting].

init_per_suite(Config)->
    application:ensure_started(oituz).


can_join_meeting(Config)->
    PeerId=11,
    MeetingId=11,
    Params=#rtc_params{},
    meck:new(sfu,[non_strict]),
    meck:expect(oituz_app,get_sfu,fun()->MeetingId)
    {ok,Peer}=rtc_peer:join_meeting(11,11,Params),

