import orka.defArity

@main
def orka2_macro_test(): Unit = {
  defArity {
    def fun(x: Int, y: Int) =
      (x + 1, y + 1, x + 10, y)
    def f(x: Int) = x
    def unit() = 2
  }

  // Compile time errors
  // defArity {
  //   def fun(x: Int, y: Int) =
  //     (x + 1, y + 1, true, y)
  //   def f(x: Int) = "hello"
  // }
}
