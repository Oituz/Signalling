-module(signalling_app_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("meck/include/meck.hrl").

all()->
    [{"can_create_connection",can_create_connection/1}].


can_create_connection(_Config)-> ...


