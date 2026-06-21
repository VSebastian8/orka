package orka
import scala.quoted.*

def orkaImpl(code: Expr[Unit])(using Quotes): Expr[Unit] = {
  val parser = OrkaParser()
  import parser.q.reflect.*

  code.asTerm match {
    case Inlined(_, _, Block(stats, _)) =>
      val (places, transitions) =
        parser.parse(
          stats.asInstanceOf[List[Statement]],
          List(),
          List()
        )
      val targetFun = transitions.head.fun
      val callTerm: Term =
        Apply(Ref(targetFun.symbol), List(Literal(IntConstant(2))))

      val rest = '{
        println("Places:\n" + ${
          Expr(places.map(p => p.name + " :: " + p.typ).mkString("\n"))
        })
        println()
        println(
          "Transitions:\n" +
            ${
              Expr(
                transitions
                  .map(tr =>
                    tr.name +
                      s" :: ${tr.inputPlaces.filterNot(_ == "").mkString(", ")} " +
                      s"|->  ${tr.outputPlaces.filterNot(_ == "").mkString(", ")}"
                  )
                  .mkString("\n")
              )
            }
        )
        println("producer(2) = " + ${ callTerm.asExprOf[Any] })
      }
      Block(transitions.map(_.fun), rest.asTerm).asExprOf[Unit]

    case t: Term =>
      report.throwError("Expected statements, found " + t.show);
  }
}

inline def orka(inline code: Unit): Unit = ${ orkaImpl('code) }
