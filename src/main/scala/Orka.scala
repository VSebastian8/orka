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

private class Function[A <: Arg](val f: A => Res)(using val tag: ClassTag[A])

object Function {
  def apply[A <: Arg, I, R](
      f: I => R
  )(using magnetRes: MagnetRes[R], magnetArg: MagnetArg[I, A])(using
      ClassTag[A]
  ): Function[A] =
    magnetRes.apply(magnetArg.apply(f))
}

// Magnet trait to deal with apply type erasure for Arg wrap
sealed trait MagnetArg[I, A <: Arg]:
  def apply[R](f: I => R): (A => R)

// Arg0
given MagnetArg[Unit, Arg0] with {
  def apply[R](f: Unit => R): Arg0 => R =
    (_: Arg0) => f(())
}
// Arg1
given MagnetArg[Int, Arg1] with {
  def apply[R](f: Int => R): Arg1 => R =
    (x: Arg1) => f(x.a)
}
// Arg2
given MagnetArg[(Int, Int), Arg2] with {
  def apply[R](f: ((Int, Int)) => R): Arg2 => R =
    (x: Arg2) => f(x.a, x.b)
}

// Magnet trait to deal with apply type erasure for Res wrap
sealed trait MagnetRes[R]:
  def apply[A <: Arg](f: A => R)(using ClassTag[A]): Function[A]

// Res0
given MagnetRes[Unit] with {
  def apply[A <: Arg](f: A => Unit)(using ClassTag[A]): Function[A] =
    new Function((x: A) => {
      f(x); Res.Res0
    })
}
// Res1
given MagnetRes[Int] with {
  def apply[A <: Arg](f: A => Int)(using ClassTag[A]): Function[A] =
    new Function((x: A) => {
      Res.Res1(f(x))
    })
}
// Res2
given MagnetRes[(Int, Int)] with {
  override def apply[A <: Arg](
      f: A => (Int, Int)
  )(using ClassTag[A]): Function[A] =
    new Function((x: A) => {
      val res = f(x)
      Res.Res2(res._1, res._2)
    })
}

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
  def producer(x: Int): (Int, Int) = {
    println("Producer with " + x); (x, x + 1)
  }
  val prod = Function(producer)

  def consumer(x: (Int, Int)): Int = {
    println("Consumer with " + x._1 + ", " + x._2); x._1 + x._2
  }
  val cons = Function(consumer)

  val orka = Orka(prod, cons, Stack(1, 2, 3))

  orka.run()
}
