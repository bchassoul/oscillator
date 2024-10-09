-module(oscillator_sensors).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([reset/0, start/0, calibrate/0]).

-define(OSCILLERL, {oscillerl, 'oscillerl@codeBeam'}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

start() ->
    cast(start).

reset() ->
    cast(reset).

calibrate() ->
    cast(calibrate).

init(_) ->
    {ok, #{acc => [], gyro => []}}.

call(Call) ->
    case gen_server:call(?MODULE, Call) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

cast(Cast) ->
    case gen_server:cast(?MODULE, Cast) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.


handle_call(measurements, _, #{acc := Acc, gyro := Gyro, acc_offset := Acc_offset}=State) ->
    ?LOG_NOTICE(#{event => measurements}),
    {reply, #{result => #{acc => Acc, gyro => Gyro, acc_offset => Acc_offset}}, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(calibrate, State) ->
    ?LOG_NOTICE(#{event => calibrate}),
    set_board(init),
    {ok, TRef} = timer:send_interval(200, collect_data_loop),
    erlang:send_after(5000, self(), offset),
    {noreply, State#{timer_ref => TRef}};
handle_cast(set, State) ->
    ?LOG_NOTICE(#{event => set}),
    set_board(set),
    {noreply, State};
handle_cast(reset, State) ->
    ?LOG_NOTICE(#{event => reset}),
    {noreply, State#{acc => [], gyro => []}};
handle_cast(start, State) ->
    ?LOG_NOTICE(#{event => start}),
    set_board(start),
    {ok, TRef} = timer:send_interval(500, collect_data_loop),
    erlang:send_after(5000, self(), stop),
    {noreply, State#{timer_ref => TRef}};
handle_cast( _, State) ->
    {noreply, State}.

handle_info(collect_data_loop, #{acc := Acc, gyro := Gyro}=State) ->
    {ok, {Acc0, Gyro0}} = nav_read(),
    NewAcc = [Acc0 | Acc],
    NewGyro = [Gyro0 | Gyro],
    {noreply, State#{acc => NewAcc, gyro => NewGyro}};
handle_info(offset, #{acc := Acc_data, timer_ref := TRef}=State) ->
    ?LOG_NOTICE(#{event => offset}),
    set_board(set),
    {ok, Acc_offset} = calculate_offset(Acc_data),
    {noreply, State#{acc_offset => Acc_offset, acc => [], gyro => []}};
handle_info(stop, #{timer_ref := TRef}=State) ->
    ?LOG_NOTICE(#{event => stop}),
    set_board(stop, TRef),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

calculate_offset(Acc_data) ->
    case Acc_data of
        [] -> {ok, {0.0, 0.0, 0.0}};
        _ ->
            {SumX, SumY, SumZ, Count} = lists:foldl(
                fun([X, Y, Z], {SX, SY, SZ, C}) ->
                    {SX + X, SY + Y, SZ + Z, C + 1}
                end, {0.0, 0.0, 0.0, 0}, Acc_data),
            AvgX = SumX / Count,
            AvgY = SumY / Count,
            AvgZ = SumZ / Count,
            {ok, {AvgX, AvgY, AvgZ}}
    end.

set_board(init) ->
    pmod_ssr:on(),
    grisp_led:color(2, yellow),
    gen_server:cast(?OSCILLERL, running);
set_board(start) ->
    pmod_ssr:off(),
    grisp_led:color(2, blue),
    gen_server:cast(?OSCILLERL, running);
set_board(set) ->
    pmod_ssr:on(),
    grisp_led:color(2, green),
    gen_server:cast(?OSCILLERL, ready).
set_board(stop, Ref) ->
    timer:cancel(Ref),
    pmod_ssr:off(),
    grisp_led:off(2),
    gen_server:cast(?OSCILLERL, done),
    gen_server:cast(?OSCILLERL, measurements).

nav_read() ->
    Acc = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl], #{xl_unit => mg}),
    Gyro = pmod_nav:read(acc, [out_x_g, out_y_g, out_z_g]),
    {ok, {Acc, Gyro}}.
