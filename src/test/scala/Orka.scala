import orka.orka

@main
def orka_test(): Unit = {
  val orca = orka {
    type p1 = Int
    type p2 = Int
    type p3 = String

    def producer1(x: p1): (p2, p2) =
      (x, x)

    def producer2(x: p1): (p2, p2) =
      (x, x + 10)

    def consumer(x: p2, y: p2): p3 = {
      s"$x == $y => ${x == y}"
    }

    def printer(str: p3, _s: Unit): Unit = {
      println(s"Result: $str")
    }
  }
  println()
  orca.run(p1 = List(1, 2, 3), p2 = List(11, 12), p3 = List("Start"))
}
