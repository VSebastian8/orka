package orka
import scala.quoted.*

def inspectArity(f: Expr[AnyRef])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  def getParams(term: Term): Option[(List[ValDef], TypeRepr)] = {
    term match {
      case Lambda(params, body) => Some((params, body.tpe.widen))
      case Inlined(_, _, expr)  => getParams(expr)
      case Block(_, expr)       => getParams(expr)
      case _                    => None
    }
  }

  def extractTupleElements(t: TypeRepr): List[TypeRepr] = {
    t.dealias match {
      case AppliedType(tp, args) if tp <:< TypeRepr.of[Tuple] =>
        args
      case _ => List(t)
    }
  }

  def handleResults(args: List[ValDef], rets: List[TypeRepr]) = {

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
          s"Return type in ${index}th position is not an Int but ${tpe.show}"
        }

    val all_errors = args_errors ++ rets_errors

    if (!all_errors.isEmpty) {
      report.error(all_errors.mkString("\n"))
    }

    '{
      println("Function has arity " + ${ Expr(args.size) } + " -> " + ${
        Expr(rets.size)
      })
    }

  }

  getParams(f.asTerm) match {
    case Some((args, ret)) => handleResults(args, extractTupleElements(ret))
    case None =>
      report.error(
        s"Could not extract the arguments types from ${f.show}. Try passing in a `lambda` or an `inline def method`"
      ); '{}
  }
}

inline def arity(inline f: AnyRef): Unit =
  ${ inspectArity('f) }

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
