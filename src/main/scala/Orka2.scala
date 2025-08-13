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
  ): Option[String] = {

    val args_errors =
      args
        .filter { param =>
          val tpe = param.tpt.tpe
          !(tpe =:= TypeRepr.of[Int])
        }
        .map { param =>
          s"Argument ${param.name} for function $name is not an Int but ${param.tpt.tpe.show}"
        }

    val rets_errors =
      rets.zipWithIndex
        .filter { (tpe, _) =>
          !(tpe =:= TypeRepr.of[Int])
        }
        .map { (tpe, index) =>
          s"Return type of function $name at index ${index} is not an Int but ${tpe.show}"
        }

    val all_errors = args_errors ++ rets_errors

    if (!all_errors.isEmpty) {
      Some(all_errors.mkString("\n"))
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
        Left(errors)
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
      report.error(errors.mkString("\n"));
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
          params.head match {
            case TermParamClause(args) =>
              handleMethod(name, args, returns.tpe)
            case _ => Left("Could not parse method arguments")
          }
        case t: Term =>
          Left("Expected def method, found " + t.show)
      }
      handleResults(results)
    //   report.error(stats.map(_.show).mkString(" ||| "))
    case t: Term => report.error("Expected statements, found " + t.show); '{}
  }

//   getParams(body.asTerm) match {
//     case Some((args, ret)) => handleResults(args, extractTupleElements(ret))
//     case None =>
//       report.error(
//         s"Could not extract the arguments types from ${body.show}. Try passing in a `lambda` or an `inline def method`"
//       ); '{}
//   }
}

inline def defArity(inline f: Unit): Unit =
  ${ inspectDefArity('f) }
