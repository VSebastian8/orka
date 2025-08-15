import orka.arity
import orka.defArity

@main
def arity_macro_test(): Unit = {
  val f1: (Int, Int) => Int = (a, b) => a * b
  inline def f2(a: Int, b: Int, c: Int) = {
    println("hi there")
    (c, b, a)
  }

  arity(f2)
  arity((_: Int) => (1, 0, 0, 1))
  arity(() => (2, 3))
  arity((x: Int) => (x, x))
  arity((x: Int, y: Int, z: Int) => x + y + z)

  // Compile-time Errors
  // arity(f1)
  // arity((z: Int, y: Int) => "hey")
  // arity((x: String, y: Boolean) => ())
}

@main
def def_macro_test(): Unit = {
  type p1 = Int

  defArity {
    def fun(x: p1)(y: Int) =
      (x + 1, y + 1, x + 10, y)
    def f(x: Int) = x
    def unit() = 2
  }

  // Compile time errors
  // defArity {
  //   def fun(x: Int, y: Int) =
  //     (x + 1, y + 1, true, y)
  //   def f(x: String) = x
  //   def unit() = "hello"
  // }
}
