% @private
% @doc oscillator top level supervisor.
-module(oscillator_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Nav = #{
        id => pmod_nav,
        start => {pmod_nav, start_link, [spi2, #{}]}
    },
    SSR = #{
        id => pmod_ssr,
        start => {pmod_ssr, start_link, [gpio1, #{}]}
    },
    Sensors = #{
        id => oscillator_sensors,
        start => {oscillator_sensors, start_link, []}
    },
    ChildSpecs = [Nav, SSR, Sensors],
    {ok, {SupFlags, ChildSpecs}}.