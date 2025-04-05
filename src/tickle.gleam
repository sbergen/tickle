import gleam/erlang/process.{type Cancelled, type Subject}
import gleam/option.{type Option, None, Some}

type Table

type ActionKey

type ScheduledAction =
  fn() -> Nil

/// Represents either a simulated, or native Erlang timer,
/// similar to `process.Timer`.
pub opaque type Timer {
  NativeTimer(process.Timer)
  SimulatedTimer(Table, ActionKey)
}

/// Represents either a simulated or native scheduler,
/// for scheduling `send_after` calls.
pub opaque type Scheduler {
  NativeScheduler
  SimulatedScheduler(Table)
}

/// Send a message over a channel after a specified number of milliseconds.
/// 
/// If the scheduler is a simulated scheduler,
/// the message will not be sent until `advance` is called,
/// even if the delay is zero.
/// Otherwise should behave the same as `process.send_after`.
pub fn send_after(
  scheduler: Scheduler,
  subject: Subject(a),
  delay: Int,
  message: a,
) -> Timer {
  case scheduler {
    NativeScheduler -> NativeTimer(process.send_after(subject, delay, message))
    SimulatedScheduler(table) -> {
      let id = add(table, delay, fn() { process.send(subject, message) })
      SimulatedTimer(table, id)
    }
  }
}

/// Cancel a given timer, causing it not to trigger if it has not done already.
///
/// Should behave the same as `process.cancel_timer`
pub fn cancel_timer(timer: Timer) -> Cancelled {
  case timer {
    NativeTimer(timer) -> process.cancel_timer(timer)
    SimulatedTimer(table, id) ->
      case cancel_timer_ffi(table, id) {
        None -> process.TimerNotFound
        Some(time_left) -> process.Cancelled(time_left)
      }
  }
}

/// Constructs a native scheduler, which will use standard Erlang scheduling.
pub fn native_scheduler() -> Scheduler {
  NativeScheduler
}

/// Runs an operation using a simulated scheduler,
/// and tears down the setup after the operation.
pub fn simulate(operation: fn(Scheduler) -> a) -> a {
  let assert Ok(table) = new_table()
  let scheduler = SimulatedScheduler(table)

  let result = operation(scheduler)

  drop_table(table)
  result
}

/// Advances a simulated scheduler, or panics if the scheduler is not simulated.
/// Calling this function concurrently leads to undefined behavior.
pub fn advance(scheduler: Scheduler, amount: Int) -> Nil {
  case scheduler {
    NativeScheduler -> panic as "Trying to advance native scheduler"
    SimulatedScheduler(table) -> advance_ffi(table, amount)
  }
}

@external(erlang, "tickle_ffi", "add")
fn add(table: Table, delay: Int, action: ScheduledAction) -> ActionKey

@external(erlang, "tickle_ffi", "new_table")
fn new_table() -> Result(Table, Nil)

@external(erlang, "tickle_ffi", "advance")
fn advance_ffi(table: Table, amount: Int) -> Nil

@external(erlang, "tickle_ffi", "cancel_timer")
fn cancel_timer_ffi(table: Table, id: ActionKey) -> Option(Int)

@external(erlang, "ets", "delete")
fn drop_table(table: Table) -> Nil
