package orka

import scala.annotation.experimental

@main
def hello(): Unit =
  orka2 {
    def f(x: Int)(y: Int) = {
      println("okk")
      (x + 1, y, y + 3)
    }
  }
  println("Orka")
