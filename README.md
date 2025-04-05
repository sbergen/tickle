# tickle

[![Package Version](https://img.shields.io/hexpm/v/tickle)](https://hex.pm/packages/tickle)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tickle/)


Tickle allows you to simulate delays in Gleam on Erlang,
when using `tickle.send_after` instead of `process.send_after`.
It also contains a mechanism for defining synchronization points
that can safely be waited on in tests.
This allows writing faster and more deterministic tests
for anything requiring delays.

```sh
gleam add tickle@1
```

```gleam
import gleam/erlang/process
import tickle

pub fn main() {
  // Creates a scheduler that can simulate delays
  use scheduler <- tickle.simulate()

  // To use real scheduling, do this instead:
  // let scheduler = tickle.native_scheduler()

  // Send some messages with delays
  let subject = process.new_subject()
  tickle.send_after(scheduler, subject, 1, "wibble")
  tickle.send_after(scheduler, subject, 2, "wobble")

  // Nothing is received yet!
  let assert Error(Nil) = process.receive(subject, 0)

  // Only the first message will trigger within one ms
  tickle.advance(scheduler, 1)
  let assert Ok("wibble") = process.receive(subject, 0)
  let assert Error(Nil) = process.receive(subject, 0)

  // ...and after another ms, the next messages triggers.
  tickle.advance(scheduler, 1)
  let assert Ok("wobble") = process.receive(subject, 0)
}
```

Additionally, since not all delays in asynchronous programs are caused by
delayed messages, `tickle` provides a mechanism to define synchronization points
that can be waited on:
* `wait_for_notify` can be used to safely trigger an action, and
* `notify` can be used downstream of the action to let the wait continue.

Using such synchronization points may be necessary before advancing a simulated
scheduler, in order to not advance time right before `send_after` is used.
`wait_for_notify` is only available with a simulated scheduler,
and `notify` will be (nearly) a no-op with a native scheduler.
