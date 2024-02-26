-module(oituz_app_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/rtp.hrl").

all()->
    [
    can_start_session,
    can_create_connection
    ].


can_start_session(_Config)->
    application:ensure_started(signalling_app),
    RTPParams=#rtp_params{
        candidates=[#ice_candidate{}],
        constraints=[#media_constraint{}],
        tracks=[#track{}]
    },
    PeerId=11,
    MeetingId=12,
    
    {ok,Pid}=signalling_app:create_connection(PeerId,MeetingId,RTPParams),
    ?assert(whereis(register_service)=/=undefined),
    ?assert(erlang:is_process_alive(Pid)=:=true).

    
can_create_connection(_Config)-> ok.
    % Connection=#rtp:rtp_connection{},
    % meck:new(signalling_app,[non_strict]),
    % meck:expect(signalling_app,create_connection,fun()->Connection end),
    % Rez=signalling_app:create_connection(),
    % meck:validate(signalling_app),
    % meck:unload(signalling_app).
    


