package orka
import scala.quoted.*

def orkaImpl(code: Expr[Unit])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  val parser = OrkaParser()

  code.asTerm match {
    case Inlined(_, _, Block(stats, _)) =>
      val (places, transitions) =
        parser.parse(
          stats.asInstanceOf[List[parser.q.reflect.Statement]],
          List(),
          List()
        )

      '{
        println("Places:\n" + ${ Expr(places.mkString(", ")) })
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
      }

    case t: Term =>
      report.throwError("Expected statements, found " + t.show);
  }
}

inline def orka(inline code: Unit): Unit = ${ orkaImpl('code) }
