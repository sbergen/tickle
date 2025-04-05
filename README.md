# tickle

<!--
[![Package Version](https://img.shields.io/hexpm/v/tickle)](https://hex.pm/packages/tickle)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/tickle/)

```sh
gleam add tickle@1
```
-->

Tickle allows you to simulate delays in Gleam on Erlang,
when using `tickle.send_after` instead of `process.send_after`.
This allows writing faster and more deterministic tests
for anything requiring delays.

```gleam
import tickle

pub fn main() {
  // Creates a scheduler that can simulate delays
  use scheduler <- tickle.simulate()

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

<!--
Further documentation can be found at <https://hexdocs.pm/tickle>.
-->
