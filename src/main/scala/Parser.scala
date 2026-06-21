package orka
import scala.quoted.*
import scala.annotation.tailrec

class OrkaParser[Q <: Quotes & Singleton](using val q: Q):
  import q.reflect.*

  // All arguments need to have the type Int
  def extractInputPlaces(
      args: List[ValDef],
      places: List[String]
  ): List[String] =
    args.flatMap((arg => {
      arg match {
        case ValDef(arg_name, arg_type, _) =>
          arg_type match {
            case TypeIdent(place) =>
              if place == "Unit"
              then Some("")
              else if places.contains(place)
              then Some(place)
              else
                report.error(
                  s"Error for argument $arg_name: required place, found $place",
                  arg.pos
                )
                None
            case other =>
              report.error(
                s"Error for argument $arg_name, expected type, found ${other
                    .toString()}",
                arg.pos
              )
              None
          }
      }
    }))

  // All return types need to be a place
  def extractOutputPlaces(
      rets: List[TypeTree],
      places: List[String]
  ): List[String] =
    rets.zipWithIndex.flatMap((ret, index) => {
      val place = ret.tpe.typeSymbol.name
      if place == "Unit"
      then Some("")
      else if places.contains(place)
      then Some(place)
      else
        report.error(
          s"Error for return type ${index}: required place, found $place",
          ret.pos
        )
        None
    })

  case class Place(
      name: String,
      typ: String,
      td: TypeDef
  )

  case class Transition(
      name: String,
      inputPlaces: List[String],
      outputPlaces: List[String],
      fun: DefDef
  )

  // Parse the statements, extracting the places and transitions
  @tailrec
  final def parse(
      stats: List[Statement],
      places: List[Place],
      transitions: List[Transition]
  ): (List[Place], List[Transition]) =
    stats match {
      case Nil => (places, transitions)
      case head :: next =>
        head match {
          case fun @ DefDef(name, params, returns, _) =>
            val args = params
              .flatMap {
                case TermParamClause(argsClause) => argsClause
                case _ =>
                  report.errorAndAbort(
                    s"Method $name has wrong parameters",
                    fun.pos
                  )
              }
            val rets = returns match {
              case Applied(tycon, args) if tycon.tpe <:< TypeRepr.of[Tuple] =>
                args.map {
                  case tt: TypeTree => tt
                  case other =>
                    report.errorAndAbort(
                      s"Unexpected type argument: ${other.show}",
                      other.pos
                    )
                }
              case _ => List(returns)
            }
            val body: Term = fun.rhs.getOrElse {
              report.errorAndAbort(s"Method $name has no body", fun.pos)
            }

            val tr = Transition(
              name,
              extractInputPlaces(args, places.map(_.name)),
              extractOutputPlaces(rets, places.map(_.name)),
              fun
            )

            parse(next, places, transitions :+ tr)
          case td @ TypeDef(name, t) =>
            t match {
              case TypeIdent(typ) =>
                parse(next, places :+ Place(name, typ, td), transitions)
              case t =>
                report.error("Expected place type, found " + t.show, t.pos)
                parse(next, places, transitions)
            }
          case t =>
            report.error("Expected def method, found " + t.show, t.pos)
            parse(next, places, transitions)
        }
    }
