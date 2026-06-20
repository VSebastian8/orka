package orka
import scala.quoted.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scala.util.Random

class OrkaUtils(using val q: Quotes):
  import q.reflect.*

  // (Int, String, Bool) -> [Int, String, Bool]
  // def extractReturnTypes(
  //     t: quotes.reflect.TypeRepr
  // )(using Quotes): List[TypeRepr] = {
  //   t.dealias match {
  //     case AppliedType(tp, args) if tp <:< TypeRepr.of[Tuple] => {
  //       args
  //     }
  //     case _ => List(t)
  //   }
  // }

  def extractReturnTypes(t: TypeTree)(using Quotes): List[TypeTree] = {
    t match {
      case Applied(tycon, args) if tycon.tpe <:< TypeRepr.of[Tuple] =>
        args.map {
          case tt: TypeTree => tt
          case other =>
            report.errorAndAbort(
              s"Unexpected type argument: ${other.show}",
              other.pos
            )
        }
      case _ =>
        List(t)
    }
  }

  case class ListLambda(
      arity: Expr[Int],
      name: Expr[String],
      lambda: Expr[List[Int] => List[Int]]
  )

  // Throw compile errors for the function's types
  def validate(
      name: String,
      args: List[ValDef],
      rets: List[TypeTree],
      places: List[String]
  ): Unit = {
    argumentErrors(args, places)
    returnErrors(rets, places)
  }

  // All arguments need to have the type Int
  def argumentErrors(args: List[ValDef], places: List[String]): Unit =
    args.foreach((arg => {
      arg match {
        case ValDef(arg_name, arg_type, _) =>
          arg_type match {
            case TypeIdent(place) =>
              if !places.contains(place)
              then
                report.error(
                  s"Error for argument $arg_name: required place, found $place",
                  arg.pos
                )
            case other =>
              report.error(
                s"Error for argument $arg_name, expected type, found ${other
                    .toString()}",
                arg.pos
              )
          }
      }
    }))

  // All return types need to be a place
  def returnErrors(rets: List[TypeTree], places: List[String]): Unit =
    rets.zipWithIndex.foreach { (ret, index) =>
      {
        val place = ret.tpe.typeSymbol.name
        if !places.contains(place)
        then
          report.error(
            s"Error for return type ${index}: required place, found $place",
            ret.pos
          )
      }
    }

  // Transform the method into a lambda function that takes its arguments as a List and returns a List instead of a Tuple
  def listifyMethod(
      paramSyms: List[Symbol],
      bodyTerm: Term,
      outputArity: Int
  ): Expr[List[Int] => List[Int]] = {
    val owner = Symbol.spliceOwner
    // Substitute the method parameters with the new lambda list argument
    def substArgs(argsParam: Term, bodyTerm: Term): Term = {
      // Input arguments from a list
      val paramMap: Map[Symbol, Term] =
        paramSyms.zipWithIndex.map { case (sym, i) =>
          sym -> Apply(
            Select.unique(argsParam, "apply"),
            List(Literal(IntConstant(i)))
          )
        }.toMap
      // Substitution of old method params with new params names
      object SubstParams extends TreeMap {
        override def transformTerm(tree: Term)(owner: Symbol): Term =
          tree match {
            case id: Ident if paramMap.contains(id.symbol) =>
              paramMap(id.symbol)
            case _ => super.transformTerm(tree)(owner)
          }
      }
      // Handle input arguments
      SubstParams.transformTerm(bodyTerm.changeOwner(owner))(owner)
    }
    // Wrap the return type into a list
    def returnBody(body: Term): Term = {
      // Handle output elements
      val returnElemSym = Symbol.newVal(
        owner,
        "returnTemp",
        body.tpe,
        Flags.EmptyFlags,
        Symbol.noSymbol
      )
      val returnElemVal = ValDef(returnElemSym, Some(body))
      // List wrap
      val elems = if (outputArity == 1) {
        // A single element will be wrapped in a List
        List(Ref(returnElemSym).asExprOf[Int])
      } else {
        // Elements in a tuple will be collected in a List
        (1 to outputArity).map { i =>
          Select
            .unique(Ref(returnElemSym), s"_$i")
            .asExprOf[Int]
        }.toList
      }
      // The calculated return values will be in scope
      Block(
        List(returnElemVal),
        Expr.ofList(elems).asTerm
      )
    }
    // Resulting lambda's type
    val mtpe = MethodType(List("args"))(
      _ => List(TypeRepr.of[List[Int]]),
      _ => TypeRepr.of[List[Int]]
    )
    // Final processed lambda
    Lambda(
      owner,
      mtpe,
      (owner, params) => {
        val List(argsParam: Term) = params: @unchecked
        val substitutedBody =
          substArgs(argsParam, bodyTerm)
        returnBody(substitutedBody)
      }
    ).asExprOf[List[Int] => List[Int]]
  }

  // Parse the statements, extracting the places and transitions
  def parseStats(
      stats: List[Statement],
      places: List[String],
      transitions: List[ListLambda]
  ): (List[String], List[ListLambda]) =
    stats match {
      case Nil => (places, transitions)
      case head :: next =>
        head match {
          case fun @ DefDef(name, params, returns, _) =>
            val args = params
              .flatMap {
                case TermParamClause(argsClause) => argsClause
                case _ =>
                  report.throwError(
                    s"Method $name has wrong parameters"
                  )
              }
            val rets = extractReturnTypes(returns)
            validate(name, args, rets, places)

            val inputArity = args.length
            val outputArity = rets.length

            val body: Term = fun.rhs.getOrElse {
              report.throwError(s"Method $name has no body")
            }

            val tr = ListLambda(
              Expr(inputArity),
              Expr(name),
              listifyMethod(args.map(_.symbol), body, outputArity)
            )
            parseStats(next, places, tr :: transitions)
          case TypeDef(place, _) =>
            parseStats(next, place :: places, transitions)
          case t =>
            report.throwError("Expected def method, found " + t.show);
        }
    }

  // Simple output function for testing
  def runOrka(
      places: List[String],
      lambdas: List[ListLambda]
  ): Expr[Queue[Int] => Unit] = {
    val funs: Expr[List[List[Int] => List[Int]]] =
      Expr.ofList(
        lambdas.map(l => '{ (xs) => ${ l.lambda }(xs) })
      )
    val arities = Expr.ofList(lambdas.map(_.arity))
    val names = Expr.ofList(lambdas.map(_.name))
    // Final generated block of code
    '{ (start: Queue[Int]) =>
      println("Places: " + ${ Expr(places.mkString(", ")) })
      val n = $arities.length

      @tailrec
      def run(res: Queue[Int]): Unit = {
        if (res.isEmpty)
          return
        // pick a random function from the list
        val index = Random().between(0, n)
        val needed = $arities(index)
        if (needed > res.length) {
          run(res)
        } else {
          println("Current resources " + res + "\n")
          Thread.sleep(500)
          // We have enough resources to call the function
          println(s"Chosen function ${$names(index)}")
          val results = $funs(index)(res.take(needed).toList)
          run(res.drop(needed).enqueueAll(results))
        }
      }

      run(start)
    }
  }

  def parse(code: Expr[Unit])(using Quotes): Expr[Queue[Int] => Unit] = {
    code.asTerm match {
      case Inlined(_, _, Block(stats, _)) =>
        val (places, transitions) =
          parseStats(stats, List(), List())

        runOrka(places, transitions)
      case t: Term =>
        report.throwError("Expected statements, found " + t.show);
    }
  }

def orkaImpl(code: Expr[Unit])(using Quotes): Expr[Queue[Int] => Unit] = {
  val utils = new OrkaUtils()
  utils.parse(code)
}

inline def orka(inline code: Unit): Queue[Int] => Unit = ${ orkaImpl('code) }
