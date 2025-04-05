import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/process.{Cancelled, TimerNotFound}
import gleeunit
import gleeunit/should
import tickle.{AlreadyWaiting, NotifyTimedOut}

pub fn main() {
  gleeunit.main()
}

pub fn send_after_test() {
  let subject = process.new_subject()
  use scheduler <- tickle.simulate()

  tickle.send_after(scheduler, subject, 2, "first")
  tickle.send_after(scheduler, subject, 2, "second")
  tickle.send_after(scheduler, subject, 3, "third")
  let assert Error(Nil) = process.receive(subject, 0)
    as "advance hasn't been called"

  tickle.advance(scheduler, 1)
  let assert Error(Nil) = process.receive(subject, 0)
    as "hasn't advanced enough for anything yet"

  tickle.advance(scheduler, 1)
  let assert Ok("first") = process.receive(subject, 0)
  let assert Ok("second") = process.receive(subject, 0)

  let assert Error(Nil) = process.receive(subject, 0)
    as "hasn't advanced enough for third yet"

  tickle.advance(scheduler, 10)
  let assert Ok("third") = process.receive(subject, 0)
}

pub fn cancel_timer_test() {
  let subject = process.new_subject()
  use scheduler <- tickle.simulate()

  let timer = tickle.send_after(scheduler, subject, 5, "wibble")
  tickle.advance(scheduler, 1)
  let assert Cancelled(4) = tickle.cancel_timer(timer)

  tickle.advance(scheduler, 10)
  let assert Error(Nil) = process.receive(subject, 0)
}

pub fn cancel_timer_not_found_test() {
  let subject = process.new_subject()

  // Check native functionality
  let timer = process.send_after(subject, 1, "wibble")
  process.sleep(1)
  let assert TimerNotFound = process.cancel_timer(timer)

  // Now check tickle
  use scheduler <- tickle.simulate()
  let timer = tickle.send_after(scheduler, subject, 1, "wobble")
  tickle.advance(scheduler, 1)
  let assert TimerNotFound = tickle.cancel_timer(timer)
}

pub fn cancel_timer_twice_test() {
  let subject = process.new_subject()
  use scheduler <- tickle.simulate()
  let timer = tickle.send_after(scheduler, subject, 1, "wibble")

  tickle.cancel_timer(timer)
  let assert TimerNotFound = tickle.cancel_timer(timer)
}

pub fn zero_delay_test() {
  let subject = process.new_subject()

  // Check native functionality
  let timer = process.send_after(subject, 0, "wibble")
  let assert Cancelled(0) = process.cancel_timer(timer)
  let assert Error(Nil) = process.receive(subject, 0)
    as "Assuming zero delay timer to be cancelable on native implementation"

  // Now check tickle
  use scheduler <- tickle.simulate()
  let timer = tickle.send_after(scheduler, subject, 0, "wobble")
  let assert Cancelled(0) = tickle.cancel_timer(timer)
  let assert Error(Nil) = process.receive(subject, 0)
}

pub fn negative_delay_test() {
  let subject = process.new_subject()

  let assert Ok(badarg) = atom.from_string("badarg")
  let badarg = dynamic.from(badarg)

  // Check native functionality
  let assert Error(erlang.Errored(err)) =
    erlang.rescue(fn() { process.send_after(subject, -1, "wibble") })
  err |> should.equal(badarg)

  // Now check tickle
  use scheduler <- tickle.simulate()
  let assert Error(erlang.Errored(err)) =
    erlang.rescue(fn() { tickle.send_after(scheduler, subject, -1, "wibble") })
  err |> should.equal(badarg)
}

pub fn multi_process_test() {
  use scheduler <- tickle.simulate()
  let subject = process.new_subject()

  process.start(
    fn() {
      process.send(subject, "spawned")
      tickle.send_after(scheduler, subject, 10, "hello from other process")
    },
    linked: True,
  )

  let assert Ok("spawned") = process.receive(subject, 10)
    as "expected process to spawn"

  let assert Error(Nil) = process.receive(subject, 0)
    as "advance hasn't been called"

  tickle.advance(scheduler, 10)
  let assert Ok("hello from other process") = process.receive(subject, 0)
}

pub fn wait_for_notify_success_test() {
  use scheduler <- tickle.simulate()

  let assert Ok(42) =
    tickle.wait_for_notify(scheduler, "foo", 10, fn() {
      process.start(fn() { tickle.notify(scheduler, "foo") }, linked: True)
      42
    })
}

pub fn wait_for_notify_timeout_test() {
  use scheduler <- tickle.simulate()

  let assert Error(NotifyTimedOut) =
    tickle.wait_for_notify(scheduler, "foo", 10, fn() { Nil })

  let assert Error(NotifyTimedOut) =
    tickle.wait_for_notify(scheduler, "foo", 10, fn() {
      process.start(fn() { tickle.notify(scheduler, "bar") }, linked: True)
    })
}

pub fn wait_for_notify_again_test() {
  use scheduler <- tickle.simulate()

  tickle.wait_for_notify(scheduler, "foo", 10, fn() {
    let assert Error(AlreadyWaiting) =
      tickle.wait_for_notify(scheduler, "bar", 10, fn() { Nil })
  })
}

pub fn notify_without_wait_test() {
  use scheduler <- tickle.simulate()

  // This shouldn't cause an error
  tickle.notify(scheduler, 42)
}

pub fn native_scheduler_smoke_test() {
  let subject = process.new_subject()
  let scheduler = tickle.native_scheduler()

  let timer1 = tickle.send_after(scheduler, subject, 10, "wibble")
  let timer2 = tickle.send_after(scheduler, subject, 20, "wobble")
  let timer3 = tickle.send_after(scheduler, subject, 30, "")

  let assert Cancelled(time) = tickle.cancel_timer(timer3)
  { time > 20 } |> should.be_true()

  let assert Error(Nil) = process.receive(subject, 0) as "shouldn't receive yet"
  let assert Ok("wibble") = process.receive(subject, 15)

  let assert TimerNotFound = tickle.cancel_timer(timer1)

  tickle.cancel_timer(timer2)
  let assert Error(Nil) = process.receive(subject, 15)
}
