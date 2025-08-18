import orka.orka
import scala.collection.immutable.Queue

@main
def orka_test(): Unit = {
  val run = orka {
    def producer(x: Int) = (x, x)
    def consumer(x: Int, y: Int) = x + y
  }
  run(Queue(1, 1))
}
