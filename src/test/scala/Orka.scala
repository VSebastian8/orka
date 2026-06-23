import orka.orka

@main
def orka_test(): Unit = {
  val orca = orka {
    type p1 = Int
    type p2 = Int
    type p3 = String

    def producer(x: p1): (p2, p2) =
      println("hey")
      (x, x)

    def consumer(x: p2, y: p2): p3 = {
      if (x == y) (x + y).toString()
      else x.toString()
    }
    def printer(str: p3, _s: Unit): Unit = {
      println(s"Result: $str")
    }
  }
  println()
  orca.run(p1 = List(1, 2, 3), p2 = Nil, p3 = List("a", "b"))
}
