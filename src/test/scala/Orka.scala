import orka.orka
import scala.collection.immutable.Queue

@main
def orka_test(): Unit = {
  val run = orka {
    type p1 = Int
    type p2 = Int
    type p3 = String
    def producer(x: p1): (p1, p2) = (x, x)
    def consumer(x: p1, y: p2): p1 = {
      if (x == y) x + y
      else x
    }
  }
  run(Queue(1, 1))
}
