import scala.collection.mutable.Stack
import scala.reflect.ClassTag
import scala.annotation.tailrec

sealed trait Arg

case class Arg0() extends Arg
case class Arg1(a: Int) extends Arg
case class Arg2(a: Int, b: Int) extends Arg

enum Res:
  case Res0
  case Res1(a: Int)
  case Res2(a: Int, b: Int)

case class Function[A <: Arg](f: A => Res)(using val tag: ClassTag[A])

def length(fn: Function[?]): Int =
  fn.tag.runtimeClass match
    case c if c == summon[ClassTag[Arg0]].runtimeClass => 0
    case c if c == summon[ClassTag[Arg1]].runtimeClass => 1
    case c if c == summon[ClassTag[Arg2]].runtimeClass => 2

class Orka[A <: Arg, B <: Arg](
    val producer: Function[A],
    val consumer: Function[B],
    var resources: Stack[Int]
) {
  private def getRes(fn: Function[? <: Arg]): Res = {
    fn.tag.runtimeClass match
      case c if c == summon[ClassTag[Arg0]].runtimeClass =>
        fn.asInstanceOf[Function[Arg0]].f(Arg0())
      case c if c == summon[ClassTag[Arg1]].runtimeClass =>
        fn.asInstanceOf[Function[Arg1]].f(Arg1(resources.pop()))
      case c if c == summon[ClassTag[Arg2]].runtimeClass =>
        fn.asInstanceOf[Function[Arg2]]
          .f(Arg2(resources.pop(), resources.pop()))
  }

  private def handleRes(res: Res): Unit = {
    res match
      case Res.Res0       => {}
      case Res.Res1(a)    => resources.push(a)
      case Res.Res2(a, b) => resources.push(a, b)
  }

  @tailrec
  final def run(): Unit = {
    val current_len = resources.length
    Thread.sleep(500)
    if (length(consumer) < current_len) {
      val res = getRes(consumer)
      handleRes(res)
      run()
    } else if (length(producer) < current_len) {
      val res = getRes(producer)
      handleRes(res)
      run()
    }
  }
}

object Orka {
  def apply[A <: Arg, B <: Arg](
      f: Function[A],
      g: Function[B],
      l: Stack[Int]
  ): Orka[A, B] =
    new Orka(f, g, l)
}

@main
def first_test(): Unit = {
  // def producer(x: Int): (Int, Int) =
  //   (x, x + 1)
  val producerF: Arg1 => Res = x => {
    println("Producer with " + x); Res.Res2(x.a, x.a + 1)
  }
  val prod = Function(producerF)

  // def consumer(x: Int, y: Int): (Int) =
  //   x + y
  val consumerF: Arg2 => Res = x => {
    println("Consumer with " + x); Res.Res1(x.a + x.b)
  }
  val cons = Function(consumerF)

  val orka = Orka(prod, cons, Stack(1, 2, 3))

  orka.run()
}
