package orka
import scala.quoted.*
import scala.util.Random
import scala.collection.immutable.Queue
import scala.annotation.tailrec

type Place = String

case class Transition(
    name: String,
    priority: Int,
    inputPlaces: List[Place],
    outputPlaces: List[(Place, Boolean)]
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
    elems: Map[String, Seq[Any] => Any],
    verbosity: Int
) extends Selectable:
  def applyDynamic(name: String)(args: Any*): Any =
    if name == "run" then
      val tokens =
        places.zip(args.map(_.asInstanceOf[List[Any]]).map(Queue.from(_))).toMap
      if (verbosity > 1) {
        println("Petri net started running!")
        println()
      }
      runOrka(tokens).map((place, toks) => (place, toks.toList))
    else elems(name)(args)

  @tailrec
  private def runOrka(tokens: Map[Place, Queue[Any]]): Map[Place, Queue[Any]] =
    if (verbosity > 2) {
      showTokens(tokens)
    }

    val active = transitions.filter(canFire(tokens, _))
    if (active.length == 0) {
      if (verbosity > 1)
        println("Petri net finished running!")
      return tokens
    } else {
      if (verbosity > 2) {
        println("Active transitions:")
        println(active.map(_.name).mkString(", "))
        println()
      }

      val tr = pickTransition(active)
      val newTokens = fireTransition(tokens, tr)
      runOrka(newTokens)
    }

  private def showTokens(tokens: Map[Place, Queue[Any]]): Unit =
    println("Tokens:")
    tokens.foreach { (place, toks) =>
      println(s"$place: [${toks.mkString(", ")}]")
    }
    println()

  // Check if a transition can fire
  private def canFire(
      tokens: Map[Place, Queue[Any]],
      transition: Transition
  ): Boolean =
    val frequencies: Map[Place, Int] =
      transition.inputPlaces
        .filterNot(_ == "")
        .foldLeft(Map.empty.withDefaultValue(0))((freq, place) =>
          freq.updatedWith(place)(v => v.map(_ + 1).orElse(Some(1)))
        )
    frequencies
      .map((place, need) => tokens(place).length >= need)
      .foldLeft(true)((res, b) => res && b)

  // Pick one transition from the active ones based on their priority
  private def pickTransition(
      trans: List[Transition]
  ): Transition =
    val maxPriority = trans.map(_.priority).max
    Random.shuffle(trans.filter(_.priority == maxPriority)).head

  private def extractTokens(
      tokens: Map[Place, Queue[Any]],
      places: List[Place]
  ): (Map[Place, Queue[Any]], Seq[Any]) =
    places match
      case Nil => (tokens, Nil)
      case p :: placesRest => {
        val (tok, toksRest) =
          if (p == "") then ((), Queue.empty) else tokens(p).dequeue
        val (newTokens, toks) =
          extractTokens(
            if (p == "") tokens else tokens.updated(p, toksRest),
            placesRest
          )
        (newTokens, tok +: toks)
      }

  private def insertTokens(
      tokens: Map[Place, Queue[Any]],
      places: List[(Place, Boolean)],
      toks: List[Any]
  ): Map[Place, Queue[Any]] = {
    places
      .zip(toks)
      .foldLeft(tokens)((acc, pair) =>
        val ((place, optional), token) = pair
        if (place == "")
        then acc
        else if optional
        then
          token.asInstanceOf[Option[Any]] match {
            case None      => acc
            case Some(tok) => acc.updatedWith(place)(_.map(_.enqueue(tok)))
          }
        else acc.updatedWith(place)(_.map(_.enqueue(token)))
      )
  }

  // Consume input tokens and create output tokens
  private def fireTransition(
      tokens: Map[Place, Queue[Any]],
      trans: Transition
  ): Map[Place, Queue[Any]] =
    val (newTokens, toks) = extractTokens(tokens, trans.inputPlaces)
    if (verbosity > 1) {
      print(s"Fire transition ${trans.name}")
      if (verbosity > 2)
        print(s" with tokens (${toks.mkString(", ")})")
      println()
    }
    val res = elems(trans.name)(toks)
    val xs: List[Any] = res match
      case t: Tuple if trans.outputPlaces.length > 1 => t.toArray.toList
      case x =>
        List(x)
    if (verbosity > 1) {
      print(s"Done transition ${trans.name}")
      if (verbosity > 2)
        print(s" with result (${xs.mkString(", ")})")
      println()
      println()
    }
    insertTokens(newTokens, trans.outputPlaces, xs)

def orkaImpl(code: Expr[Unit])(using q: Quotes): Expr[Any] = {
  import q.reflect.*
  val parser = OrkaParser[q.type](using q)
  val net = OrkaNet[q.type](using q)

  code.asTerm match {
    case Inlined(_, _, Block(stats, _)) =>
      val parser.ParserData(places, transitions, verbosity) =
        parser.parse(
          stats.asInstanceOf[List[Statement]],
          parser.ParserData(Nil, Nil, 0)
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
        Orka($placesExpr, $transitionsExpr, $mapExpr, ${ Expr(verbosity) })
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
          _ => TypeRepr.of[Map[Place, List[Any]]]
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

      val context =
        if (verbosity > 0)
        then typeDefs ++ funs :+ net.debugNet(places, transitions)
        else typeDefs ++ funs

      Block(
        context,
        ascribed.asTerm
      ).asExprOf[Any]

    case t: Term =>
      report.throwError(
        "Expected statements, found " + t.show
      )
  }
}

transparent inline def orka(inline code: Unit): Any = ${ orkaImpl('code) }
