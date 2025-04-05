-module(tickle_ffi).

-export([new_table/0, add/3, advance/2, cancel_timer/2, wait_for_notify/5, notify/2]).

-define(ID_KEY, {place_last, tickle_id}).
-define(TIME_KEY, {place_last, tickle_time}).
-define(notify_key(Value), {tickle_notify, Value}).

new_table() ->
    try
        Table = ets:new(tickle_actions, [ordered_set, public]),
        % Use tuples instead of atoms, so that first works correctly
        ets:insert(Table, {?ID_KEY, 0}),
        ets:insert(Table, {?TIME_KEY, 0}),
        {ok, Table}
    catch
        error:badarg ->
            {error, nil}
    end.

add(Table, Delay, Action) when Delay >= 0 ->
    Id = ets:update_counter(Table, ?ID_KEY, 1),
    [{_, TimeNow}] = ets:lookup(Table, ?TIME_KEY),
    Key = {TimeNow + Delay, Id},
    ets:insert(Table, {Key, Action}),
    Key;
add(_Table, _Delay, _Action) ->
    error(badarg).

cancel_timer(Table, Key) ->
    case ets:take(Table, Key) of
        [_] ->
            [{_, TimeNow}] = ets:lookup(Table, ?TIME_KEY),
            {Time, _} = Key,
            {some, Time - TimeNow};
        _ ->
            none
    end.

advance(Table, Amount) ->
    execute(Table, ets:update_counter(Table, ?TIME_KEY, Amount)).

execute(Table, TimeNow) ->
    case ets:first(Table) of
        {Time, _} = Key when Time =< TimeNow ->
            [{_, Action}] = ets:take(Table, Key),
            Action(),
            execute(Table, TimeNow);
        _ ->
            nil
    end.

wait_for_notify(Pid, Table, Value, Timeout, Trigger) ->
    Key = ?notify_key(Value),
    case ets:insert_new(Table, {Key, {Pid, Value}}) of
        true ->
            Result = Trigger(),
            ReturnValue =
                receive
                    Key ->
                        {ok, Result}
                after Timeout ->
                    {error, notify_timed_out}
                end,
            ets:delete(Table, Key),
            ReturnValue;
        false ->
            {error, already_waiting}
    end.

notify(Table, Value) ->
    Key = ?notify_key(Value),
    case ets:lookup(Table, Key) of
        [{_, {Pid, Value}}] ->
            Pid ! Key;
        _ ->
            nil
    end.
