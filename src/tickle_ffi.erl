-module(tickle_ffi).

-export([new_table/0, add/3, advance_ffi/2, cancel_timer_ffi/2]).

-define(ID_KEY, {place_last, tickle_id}).
-define(TIME_KEY, {place_last, tickle_time}).

new_table() ->
    try
        Table = ets:new(tickle_actions, [ordered_set]),
        % Use tuples instead of atoms, so that first works correctly
        ets:insert(Table, {?ID_KEY, 0}),
        ets:insert(Table, {?TIME_KEY, 0}),
        {ok, Table}
    catch
        error:badarg -> {error, nil}
    end.

add(Table, Delay, Action) when Delay >= 0 ->
    Id = ets:update_counter(Table, ?ID_KEY, 1),
    [{_, TimeNow}] = ets:lookup(Table, ?TIME_KEY),
    Key = { TimeNow + Delay, Id },
    ets:insert(Table, { Key, Action }),
    Key;
add(_Table, _Delay, _Action) -> error(badarg).

cancel_timer_ffi(Table, Key) ->
    case ets:take(Table, Key) of
        [_] ->
            [{_, TimeNow}] = ets:lookup(Table, ?TIME_KEY),
            { Time, _ } = Key,
            { some, Time - TimeNow };
        _ -> none
    end.

advance_ffi(Table, Amount) ->
    execute(Table, ets:update_counter(Table, ?TIME_KEY, Amount)).

execute(Table, TimeNow) ->
    case ets:first(Table) of 
        { Time, _ } = Key when Time =< TimeNow ->
            [{_, Action}] = ets:take(Table, Key),
            Action(),
            execute(Table, TimeNow);
        _ -> nil
    end.