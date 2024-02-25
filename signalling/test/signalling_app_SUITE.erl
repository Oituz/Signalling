-module(signalling_app_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("../include/rtp.hrl").


all()->
    {[{"can_create_connection",can_create_connection/1}]}.


can_create_connection(_Config)-> 
    Connection=#rtp:rtp_connection{},
    meck:new(signalling_app,[non_strict]),
    meck:expect(signalling_app,create_connection,fun()->Connection end),
    Rez=signalling_app:create_connection(),
    meck:validate(signalling_app),
    meck:unload(signalling_app).
    


