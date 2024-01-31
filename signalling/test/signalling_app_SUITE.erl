-module(signalling_app_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("rtp.hrl").


all()->
    {[{"can_create_connection",can_create_connection/1}]}.


can_create_connection(_Config)-> 
    meck:new(signalling_server,create_connection,)


