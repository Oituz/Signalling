-module(signalling_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,start_worker/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Args)->
     supervisor:start_child(?MODULE, [Args]).      


init(_Args) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => some_worker,
            start => {signalling_worker, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [some_worker]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.

