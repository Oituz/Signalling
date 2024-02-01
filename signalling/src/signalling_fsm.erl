-module(signalling_fsm).
-behaviour(gen_statem).

-export([stop/0, start_link/0,handle_signalling_message/2,get_state/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).


-define(Name, ?MODULE).
%% API
-record(state, {
    current_state=idle,
    connection
}).

%-----------------------API---------------------------------------
-spec get_state(Pid::pid())->{ok,State::atom()}.

get_state(Pid)->
    gen_statem:call(Pid,get_state).

-spec handle_signalling_message(Pid::pid(),Message::any())->any().
handle_signalling_message(Pid,Message)->
    gen_statem:call(Pid, Message).


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



