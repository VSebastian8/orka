package orka
import scala.quoted.*

class Orka(elems: Map[String, Seq[Any] => Any]) extends Selectable:
  def applyDynamic(name: String)(args: Any*): Any = elems(name)(args)

def orkaImpl(code: Expr[Unit])(using q: Quotes): Expr[Any] = {
  import q.reflect.*
  val parser = OrkaParser[q.type](using q)
  val net = OrkaNet[q.type](using q)

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

      val typeDefs: List[Statement] =
        places.map(_.td.changeOwner(Symbol.spliceOwner))
      val funs: List[Statement] =
        transitions.map(_.fun.changeOwner(Symbol.spliceOwner))

      val pairs: List[Expr[(String, Seq[Any] => Any)]] =
        transitions.map(t =>
          '{ (${ Expr(t.fun.name) }, ${ net.buildAdapter(t.fun) }) }
        )

      val mapExpr: Expr[Map[String, Seq[Any] => Any]] = '{
        Map(${ Varargs(pairs) }*)
      }
      val orcaExpr: Expr[Orka] = '{ Orka($mapExpr) }

      val refinedType: TypeRepr = transitions.foldLeft(TypeRepr.of[Orka]) {
        (acc, t) =>
          val pTypes =
            t.fun.termParamss.flatMap(_.params).map(_.tpt.tpe.dealias)
          val pNames = t.fun.termParamss.flatMap(_.params).map(_.name)
          val rType = t.fun.returnTpt.tpe.dealias
          Refinement(
            acc,
            t.fun.name,
            MethodType(pNames)(_ => pTypes, _ => rType)
          )
      }

      val ascribed: Expr[Any] = refinedType.asType match {
        case '[refined] => '{ $orcaExpr.asInstanceOf[refined] }
      }

      Block(typeDefs ++ funs :+ rest.asTerm, ascribed.asTerm).asExprOf[Any]

    case t: Term =>
      report.throwError("Expected statements, found " + t.show);
  }
}

transparent inline def orka(inline code: Unit): Any = ${ orkaImpl('code) }
