import gleam/erlang/process
import gleeunit
import tickle

pub fn main() {
  gleeunit.main()
}

pub fn send_after_simulated_exact_test() {
  use scheduler <- tickle.simulate()
  let subject = process.new_subject()

  tickle.send_after(scheduler, subject, 2, "a message")
  let assert Error(Nil) = process.receive(subject, 0)
    as "advance hasn't been called"

  tickle.advance(scheduler, 1)
  let assert Error(Nil) = process.receive(subject, 0) as "didn't advance enough"

  tickle.advance(scheduler, 1)
  let assert Ok("a message") = process.receive(subject, 0)
}
