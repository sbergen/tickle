import gleam/erlang/process
import gleeunit
import tickle.{Cancelled}

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
