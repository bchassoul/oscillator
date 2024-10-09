% @doc oscillator public API.
-module(oscillator_app).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) ->
  grisp_led:color(1, blue),
  grisp_led:off(2),
  oscillator_sup:start_link().

% @private
stop(_State) -> ok.
