import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/process
import gleeunit
import gleeunit/should
import tickle.{Cancelled, TimerNotFound}

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
  let assert process.TimerNotFound = process.cancel_timer(timer)

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
  let assert process.Cancelled(0) = process.cancel_timer(timer)
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
