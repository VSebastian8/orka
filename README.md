## ORKA

The name comes from the animal and also from the word `orchestrator`, with the suffix `KA` as a nod to the `AKKA` library that will be used underneath.

This is an opportunity for learning reflection based techniques in Scala for implementing a concurrency system that hides its complexity. I will build this DSL one feature at a time, starting with simpler features and abstraction and working with one reflection at a time.

### First specification

In this first DSL, there will be only two functions, a `producer` and a `consumer`. They will both have a **variable amount** of `Int` arguments (0, 1, 2) and they will return zero, one or two `Ints`. The orchestrator will have a list of integers that will be supplied to the functions `in order`, but only when there will be enough arguments in the list for a function to activate.

```scala
def producer(x: Int): (Int, Int) = {
  println("Producer with " + x);
  (x, x + 1)
}

def consumer(x: Int, y: Int): Int = {
  println("Consumer with " + x + ", " + y);
  x + y
}

val orka = Orka(producer, consumer, Stack(1, 2, 3))
orka.run()
```

### Second specification

The seconds DSL is a variation of the first one, but more generic. It is implemented with **macros** for **compile-time reflection**, which offers `more flexibility` and `better error handling`. The **orka2** macro accepts a block of method definitions using `def`. The methods now accept any number of `Int` arguments in however many `parameter clauses`. The return type is also can return any number of `Ints` in a tuple. Finally, the int resources are stored in a queue and the functions are chosen `nondeterministically`.

```scala
  val run = orka2 {
    def producer(x: Int) = (x, x)
    def consumer(x: Int, y: Int) = x + y
  }
  run(Queue(1, 1))
```
