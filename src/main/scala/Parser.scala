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

  def prettyType(t: Tree): String =
    t match {
      case TypeIdent(typ) => typ
      case Applied(x, types) =>
        val name = x.symbol.name
        if name.startsWith("Tuple")
        then s"(${types.map(prettyType).mkString(", ")})"
        else if name.startsWith("Function")
        then
          (if types.length > 2 then "(" else "")
            + s"${types.init
                .map(prettyType)
                .mkString(", ")}"
            + (if types.length > 2 then ")" else "")
            + s" => ${prettyType(types.last)}"
        else s"$name[${types.map(prettyType).mkString(", ")}]"
      case t =>
        report.errorAndAbort("Expected place type, found " + t.show, t.pos)
    }

  case class PlaceData(
      name: String,
      typ: String,
      td: TypeDef
  )

  case class TransitionData(
      name: String,
      inputPlaces: List[String],
      outputPlaces: List[String],
      fun: DefDef
  )

  // Parse the statements, extracting the places and transitions
  @tailrec
  final def parse(
      stats: List[Statement],
      places: List[PlaceData],
      transitions: List[TransitionData]
  ): (List[PlaceData], List[TransitionData]) =
    stats match {
      case Nil => (places, transitions)
      case head :: next =>
        head match {
          case td @ TypeDef(name, t) =>
            parse(
              next,
              places :+ PlaceData(name, prettyType(t), td),
              transitions
            )

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

            val tr = TransitionData(
              name,
              extractInputPlaces(args, places.map(_.name)),
              extractOutputPlaces(rets, places.map(_.name)),
              fun
            )

            parse(next, places, transitions :+ tr)

          case t =>
            report.error("Expected def method, found " + t.show, t.pos)
            parse(next, places, transitions)
        }
    }
