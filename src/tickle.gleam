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

pub type NotifyError {
  NotifyTimedOut
  AlreadyWaiting
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
      let key = add(table, delay, fn() { process.send(subject, message) })
      SimulatedTimer(table, key)
    }
  }
}

/// Cancel a given timer, causing it not to trigger if it has not done already.
///
/// Should behave the same as `process.cancel_timer`
pub fn cancel_timer(timer: Timer) -> Cancelled {
  case timer {
    NativeTimer(timer) -> process.cancel_timer(timer)
    SimulatedTimer(table, key) ->
      case cancel_timer_ffi(table, key) {
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

/// Wait for another process to call `notify` on this scheduler
/// with the given value, after running the given trigger.
/// Only one wait can be active at once on the same value.
/// Trying to wait for the same value from multiple processes
/// is suspect to race conditions.
/// Returns the value of the trigger, or an error.
/// Will panic if called on a native scheduler.
pub fn wait_for_notify(
  scheduler: Scheduler,
  value: a,
  timeout: Int,
  trigger: fn() -> b,
) -> Result(b, NotifyError) {
  case scheduler {
    NativeScheduler -> panic as "wait_for_notify on native scheduler"
    SimulatedScheduler(table) ->
      wait_for_notify_ffi(process.self(), table, value, timeout, trigger)
  }
}

/// Notifies any active waits on the given value on a simulated scheduler.
/// Does nothing on a native scheduler.
pub fn notify(scheduler: Scheduler, value: a) -> Nil {
  case scheduler {
    NativeScheduler -> Nil
    SimulatedScheduler(table) -> notify_ffi(table, value)
  }
}

/// Executes the given function, and then calls `notify` with the given arguments.
/// Example:
/// ```gleam
/// use <- tickle.deferred_notify(scheduler, value)
/// use_value(value)
/// ```
pub fn deferred_notify(scheduler: Scheduler, value: a, after: fn() -> b) -> b {
  let result = after()
  notify(scheduler, value)
  result
}

@external(erlang, "tickle_ffi", "add")
fn add(table: Table, delay: Int, action: ScheduledAction) -> ActionKey

@external(erlang, "tickle_ffi", "new_table")
fn new_table() -> Result(Table, Nil)

@external(erlang, "tickle_ffi", "advance")
fn advance_ffi(table: Table, amount: Int) -> Nil

@external(erlang, "tickle_ffi", "cancel_timer")
fn cancel_timer_ffi(table: Table, key: ActionKey) -> Option(Int)

@external(erlang, "tickle_ffi", "wait_for_notify")
fn wait_for_notify_ffi(
  pid: process.Pid,
  table: Table,
  value: a,
  timeout: Int,
  trigger: fn() -> b,
) -> Result(b, NotifyError)

@external(erlang, "tickle_ffi", "notify")
fn notify_ffi(table: Table, value: a) -> Nil

@external(erlang, "ets", "delete")
fn drop_table(table: Table) -> Nil
