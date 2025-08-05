import scala.collection.mutable.Stack
import orka.Orka1
import orka.given
import orka.arity

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
def orka1_macro_test(): Unit = {
  arity(() => (2, 3))
  arity((x: Int) => (x, x))
  arity((z: Int, y: Int) => "hey")
  arity((x: Int, y: Int, z: Int) => x + y + z)
  // arity((x: String, y: Boolean) => ())
}
