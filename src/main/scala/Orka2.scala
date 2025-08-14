package orka
import scala.quoted.*

def inspectDefArity(body: Expr[Unit])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  def extractTupleElements(t: TypeRepr): List[TypeRepr] = {
    t.dealias match {
      case AppliedType(tp, args) if tp <:< TypeRepr.of[Tuple] =>
        args
      case _ => List(t)
    }
  }

  def handleErrors(
      name: String,
      args: List[ValDef],
      rets: List[TypeRepr]
  ): Option[List[String]] = {

    val args_errors =
      args
        .filter { param =>
          val tpe = param.tpt.tpe
          !(tpe =:= TypeRepr.of[Int])
        }
        .map { param =>
          s"Argument ${param.name} is not an Int but ${param.tpt.tpe.show}"
        }

    val rets_errors =
      rets.zipWithIndex
        .filter { (tpe, _) =>
          !(tpe =:= TypeRepr.of[Int])
        }
        .map { (tpe, index) =>
          s"Return type at index ${index} is not an Int but ${tpe.show}"
        }

    val all_errors = args_errors ++ rets_errors

    if (!all_errors.isEmpty) {
      Some(all_errors)
    } else {
      None
    }
  }

  def handleMethod(
      name: String,
      args: List[ValDef],
      ret: TypeRepr
  ): Either[String, String] = {
    val rets = extractTupleElements(ret)
    handleErrors(name, args, rets) match {
      case Some(errors) =>
        Left(s"Errors for function $name:\n" + errors.mkString("\n"))
      case None =>
        Right(s"Function $name has arity ${args.size} -> ${rets.size}")
    }
  }

  def handleResults(results: List[Either[String, String]]) = {
    def errors = results.flatMap(r =>
      r match {
        case Left(err) => Some(err)
        case Right(_)  => None
      }
    )
    if (!errors.isEmpty) {
      report.error(errors.mkString("\n\n"));
      '{}
    } else {
      val messages = results
        .flatMap(r =>
          r match {
            case Left(_)    => None
            case Right(msg) => Some(msg)
          }
        )
        .mkString("\n")
      '{ println(${ Expr(messages) }) }
    }
  }

  body.asTerm match {
    case Inlined(_, _, Block(stats, _)) =>
      val results = stats.map {
        case DefDef(name, params, returns, body) =>
          val args = params
            .flatMap {
              case TermParamClause(argsClause) => argsClause
              case _ => report.error("Could not parse method arguments"); List()
            }
          handleMethod(name, args, returns.tpe)

        case t: Term =>
          Left("Expected def method, found " + t.show)
      }
      handleResults(results)
    case t: Term => report.error("Expected statements, found " + t.show); '{}
  }
}

inline def defArity(inline f: Unit): Unit =
  ${ inspectDefArity('f) }
