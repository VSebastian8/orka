package orka
import scala.quoted.*

type Place = String

case class Transition(
    name: String,
    priority: Int,
    inputPlaces: List[Place],
    outputPlaces: List[Place]
)

given ToExpr[Transition] with
  def apply(t: Transition)(using Quotes): Expr[Transition] =
    '{
      Transition(
        ${ Expr(t.name) },
        ${ Expr(t.priority) },
        ${ Expr(t.inputPlaces) },
        ${ Expr(t.outputPlaces) }
      )
    }

class Orka(
    places: List[Place],
    transitions: List[Transition],
    elems: Map[String, Seq[Any] => Any]
) extends Selectable:
  def runOrka(tokens: Map[String, List[Any]]): Unit =
    println("Tokens:")
    println(tokens)
    println("Transitions:")
    println(transitions)

  def applyDynamic(name: String)(args: Any*): Any =
    if name == "run" then
      val tokens = places.zip(args.map(_.asInstanceOf[List[Any]])).toMap
      runOrka(tokens)
    else elems(name)(args)

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

      val typeDefs: List[Statement] =
        places.map(_.td.changeOwner(Symbol.spliceOwner))

      val funs: List[Statement] =
        transitions.map(_.fun.changeOwner(Symbol.spliceOwner))

      val pairs: List[Expr[(String, Seq[Any] => Any)]] =
        transitions.map(t =>
          '{ (${ Expr(t.fun.name) }, ${ net.buildAdapter(t.fun) }) }
        )

      val placesExpr: Expr[List[String]] =
        Expr.ofList(places.map(p => Expr(p.name)))

      val transitionsExpr: Expr[List[Transition]] =
        Expr.ofList(
          transitions.map(t =>
            Expr(
              Transition(
                t.name,
                t.inputPlaces.length,
                t.inputPlaces,
                t.outputPlaces
              )
            )
          )
        )

      val mapExpr: Expr[Map[String, Seq[Any] => Any]] = '{
        Map(${ Varargs(pairs) }*)
      }

      val orcaExpr: Expr[Orka] = '{
        Orka($placesExpr, $transitionsExpr, $mapExpr)
      }

      val refinedTransitions =
        transitions.foldLeft(TypeRepr.of[Orka]) { (acc, t) =>
          val pTypes =
            t.fun.termParamss.flatMap(_.params).map(_.tpt.tpe.dealias)

          val pNames =
            t.fun.termParamss.flatMap(_.params).map(_.name)

          val rType =
            t.fun.returnTpt.tpe.dealias

          Refinement(
            acc,
            t.fun.name,
            MethodType(pNames)(_ => pTypes, _ => rType)
          )
        }

      val runType =
        MethodType(places.map(_.name))(
          _ =>
            places.map { p =>
              AppliedType(
                TypeRepr.of[List],
                List(p.td.symbol.typeRef)
              )
            },
          _ => TypeRepr.of[Unit]
        )

      val refinedType =
        Refinement(
          refinedTransitions,
          "run",
          runType
        )

      val ascribed: Expr[Any] =
        refinedType.asType match
          case '[refined] =>
            '{ $orcaExpr.asInstanceOf[refined] }

      Block(
        typeDefs ++ funs :+ net.debugNet(places, transitions),
        ascribed.asTerm
      ).asExprOf[Any]

    case t: Term =>
      report.throwError(
        "Expected statements, found " + t.show
      )
  }
}

transparent inline def orka(inline code: Unit): Any = ${ orkaImpl('code) }
