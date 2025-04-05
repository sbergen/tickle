import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}

type Table

type ActionKey

type ScheduledAction =
  fn() -> Nil

pub opaque type Timer {
  NativeTimer(process.Timer)
  SimulatedTimer(Table, ActionKey)
}

pub opaque type Scheduler {
  NativeScheduler
  SimulatedScheduler(Table)
}

pub type Cancelled {
  TimerNotFound
  Cancelled(time_remaining: Int)
}

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

pub fn cancel_timer(timer: Timer) -> Cancelled {
  case timer {
    NativeTimer(timer) ->
      case process.cancel_timer(timer) {
        process.Cancelled(time_left) -> Cancelled(time_left)
        process.TimerNotFound -> TimerNotFound
      }
    SimulatedTimer(table, id) ->
      case cancel_timer_ffi(table, id) {
        None -> TimerNotFound
        Some(time_left) -> Cancelled(time_left)
      }
  }
}

pub fn native_scheduler() -> Scheduler {
  NativeScheduler
}

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

@external(erlang, "tickle_ffi", "advance_ffi")
fn advance_ffi(table: Table, amount: Int) -> Nil

@external(erlang, "tickle_ffi", "cancel_timer_ffi")
fn cancel_timer_ffi(table: Table, id: ActionKey) -> Option(Int)

@external(erlang, "ets", "delete")
fn drop_table(table: Table) -> Nil
