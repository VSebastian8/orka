## ORKA

The name comes from the animal and also from the word `orchestrator`, with the suffix `KA` as a nod to the `AKKA` library that will be used underneath.

This is an opportunity for learning reflection based techniques in Scala for implementing a concurrency system that hides its complexity. I will build this DSL in progressive specifications, starting with simpler features and abstraction and adding one reflection at a time.

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

The seconds DSL is a variation of the first one, but more generic. It is implemented with **macros** for **compile-time reflection**, which offers `more flexibility` and `better error handling`. The **orka** macro accepts a block of method definitions using `def`. The methods now accept any number of `Int` arguments in however many `parameter clauses`. The functions can also return any number of `Ints` in a tuple. Finally, the int resources are stored in a queue and the functions are chosen `nondeterministically`.

```scala
  val run = orka {
    def producer(x: Int) = (x, x)
    def consumer(x: Int, y: Int) = x + y
  }
  run(Queue(1, 1))
```

### Third specification

The user now specifies the `places` and their token types using `type definitions` and must use these type aliases in the function definitions representing the `transitions`. The use of the place types is enforced through **compile-time parsing errors**. The transitions now have a `firing priority` based on the functions arity. This design choice makes sense from a practical perspective and can also be sidestepped by adding `dummy` Unit parameters to increase the transition's priority. The output Unit types are useful for transitions that only consume tokens without producing any.

The transition functions now work with `any` types. The macro returns a `statically typed` object with all the defined functions, as well as a `run` function that takes in the initial tokens for each place. Transitions fire automatically in order of priority (nonderministic for equal priority) when there are enough tokens for them to be enabled. A fired transition consumes the tokens from their input places and produces new tokens into the output places. When no more transitions can be fired, the function returns the current tokens in each place.

```scala
val net = orka {
    type p1 = Int
    type p2 = Int
    type p3 = String

    def producer1(x: p1): (p2, p2) =
      (x, x)

    def producer2(x: p1): (p2, p2) =
      (x, x + 10)

    def consumer(x: p2, y: p2): p3 =
      s"$x == $y => ${x == y}"


    def printer(str: p3, _s: Unit): Unit =
      println(s"Result: $str")
  }
  net.run(p1 = List(1, 2, 3), p2 = List(11, 12), p3 = List("Start"))
```

```bash
Places:
p1 :: Int
p2 :: Int
p3 :: String

Transitions:
producer1 :: p1 |->  p2, p2
producer2 :: p1 |->  p2, p2
consumer :: p2, p2 |->  p3
printer :: p3 |->

Result: Start
Result: 11 == 12 => false
Result: 1 == 1 => true
Result: 2 == 12 => false
Result: 3 == 13 => false

Done!
```

This third specification already simulates all details of an Petri Net with information tokens. The next DSL specifications build on top of this one, adding more features.
