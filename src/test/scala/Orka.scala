import scala.collection.mutable.Stack
import orka.Orka1
import orka.given
import orka.orka2

@main
def orka1_test(): Unit = {
  def prod(): Int = { println("Nothin to produce"); 2 }

  def producer(x: Int): (Int, Int) = {
    println("Producer with " + x);
    (x, x + 1)
  }

  def consumer(x: Int, y: Int): Int = {
    println("Consumer with " + x + ", " + y);
    x + y
  }

  val orka = Orka1(producer, consumer, Stack(1, 2, 3))
  orka.run()
}

@main
def orka2_test(): Unit = {
  orka2 {
    def producer(x: Int) = (x, x + 1)
    def consumer(x: Int, y: Int) = x + y
  }
}
