import gleam/erlang/process.{type Subject}

type Table

type ScheduledAction =
  fn() -> Nil

pub opaque type Timer {
  NativeTimer(process.Timer)
  SimulatedTimer(Int)
}

pub opaque type Scheduler {
  NativeScheduler
  SimulatedScheduler(Table)
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
      SimulatedTimer(id)
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
fn add(table: Table, delay: Int, action: ScheduledAction) -> Int

@external(erlang, "tickle_ffi", "new_table")
fn new_table() -> Result(Table, Nil)

@external(erlang, "tickle_ffi", "advance_ffi")
fn advance_ffi(table: Table, amount: Int) -> Nil

@external(erlang, "ets", "delete")
fn drop_table(table: Table) -> Nil
