## ORKA

The name comes from the animal and also from the word `orchestrator`, with the suffix `KA` as a nod to the `AKKA` library that will be used underneath.

This is an opportunity for learning reflection based techniques in Scala for implementing a concurrency system that hides its complexity. I will build this DSL one feature at a time, starting with simpler features and abstraction and working with one reflection at a time.

### First specification

In this first DSL, there will be only two functions, a `producer` and a `consumer`. They will both have a **variable amount** `Int` arguments and they will return one or more `Ints`. The orchestrator will have a list of integers that will be supplied to the functions nondeterministically, but only when there will be enough arguments in the list for a function to activate.

```scala
Orka(producer: x => (x, x + 1, x + 2), consumer: (x, y) => x + y, resources: List(1, 2, 3))
Orka.run()
```
