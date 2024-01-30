-module(signalling_fsm).
-behaviour(gen_statem).

-export([stop/0, start_link/0]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).


-define(Name, ?MODULE).
%% API
-record(state, {
    current_state=idle,
    connection
}).

%-----------------------API---------------------------------------
-spec handle_signalling_message(Pid::pid(),Message::any())->any().
handle_signalling_message(Pid,Message)->
    gen_fsm:send_event(Pid,{signalling_message,Message}).



stop() ->
    gen_statem:stop(?MODULE).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, state, []}.

%----------------------------------------------------------------
%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
    handle_event_function.

handle_event(enter, _OldState, _State, _Data) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.



