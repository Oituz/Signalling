-module(signalling_fsm_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).
-export([start/1]).

start(FsmData)->
    supervisor:start_child(?MODULE, [FsmData]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => some_worker,
            start => {some_worker, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [some_worker]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
