%%%-------------------------------------------------------------------
%% @doc signalling top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(oituz_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->

    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        #{
          id=>pg,
          start=>{pg,start_link,[]},
          restart=>permanent,
          shutdown=>5000,
          type=>worker,
          modules=>[pg]
        },
        #{
          id => sfu_sup,
          start=>{sfu_sup,start_link,[]},
          restart=>permanent,
          shutdown=>1000,
          type=>worker,
          modules => [sfu_sup]
        },
         #{
          id => rtc_peer_sup,
          start=>{rtc_peer_sup,start_link,[]},
          restart=>permanent,
          shutdown=>1000,
          type=>worker,
          modules => [peer_sup]
        },
         
           #{
          id => register_service,
          start=>{register_service,start_link,[]},
          restart=>permanent,
          shutdown=>1000,
          type=>worker,
          modules => [register_service]
        }

    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
