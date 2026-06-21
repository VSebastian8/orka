import orka.orka
import scala.collection.immutable.Queue

@main
def orka_test(): Unit = {
  orka {
    type p1 = Int
    type p2 = Int
    type p3 = String

    def producer(x: p1): (p2, p2) = (x, x)
    def consumer(x: p2, y: p2): p3 = {
      if (x == y) (x + y).toString()
      else x.toString()
    }
    def printer(num: p3, _s: Unit): Unit = {
      println(s"Result: $num")
    }
  }
}
