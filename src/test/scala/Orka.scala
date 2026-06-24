import orka.orka

import munit.FunSuite

class OrkaTest extends FunSuite {

  test("adder") {
    // Add all numbers from p1 and send the result to p2
    val adder_net = orka {
      // The two Int places
      type p1 = Int
      type p2 = Int
      // Take 2 numbers from p1 and add them together
      def add(x: p1, y: p1): p1 =
        x + y
      // When only one element remains in p1, send it to p2
      def ret(x: p1): p2 =
        x
    }
    // Run the net
    val endToks = adder_net.run(p1 = List(1, 2, 3, 4, 5), p2 = Nil)
    // Petri Net has finished running
    assertEquals(endToks("p1"), Nil)
    assertEquals(endToks("p2"), List(15))
  }

  test("zipper") {
    // Zip two places together
    val zip_net = orka {
      // Zip a String and an Int together in a third place
      type fruit = String
      type quant = Int
      type basket = (String, Int)
      // Test how different types are shown
      type huh = (Map[String, Int], (Option[Int], Boolean) => List[Int])
      // Take 2 numbers from p1 and add them together
      def zip(fruit: fruit, q: quant): basket =
        (fruit, q)
    }
    // Run the net
    val endToks = zip_net.run(
      fruit = List("banana", "orange", "apple"),
      quant = List(1, 2, 3, 4),
      basket = Nil,
      huh = Nil
    )
    // Petri Net has finished running
    assertEquals(endToks("fruit"), Nil)
    assertEquals(endToks("quant"), List(4))
    assertEquals(
      endToks("basket"),
      List(("banana", 1), ("orange", 2), ("apple", 3))
    )
  }
}
