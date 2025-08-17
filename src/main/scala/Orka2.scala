package orka
import scala.quoted.*

def orka2Impl(code: Expr[Unit])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  case class ListLambda(
      arity: Int,
      name: String,
      lambda: Expr[List[Int] => List[Int]]
  )

  // (Int, String, Bool) -> [Int, String, Bool]
  def extractReturnTypes(t: TypeRepr): List[TypeRepr] = {
    t.dealias match {
      case AppliedType(tp, args) if tp <:< TypeRepr.of[Tuple] =>
        args
      case _ => List(t)
    }
  }

  // Returns compile errors (if any)
  def validate(name: String, args: List[ValDef], rets: List[TypeRepr]): Unit = {
    val typeErrors = argumentErrors(args) ++ returnErrors(rets)
    if (!typeErrors.isEmpty) {
      report.throwError(
        s"Errors for function $name:\n" + typeErrors.mkString("\n")
      )
    }
  }

  // All arguments need to have the type Int
  def argumentErrors(args: List[ValDef]): List[String] =
    args
      .filterNot(_.tpt.tpe =:= TypeRepr.of[Int])
      .map(v =>
        "Error at Argument " + v.name + ": Required - Int, Found - " + v.tpt.tpe.show
      )

  // All return types need to be Int
  def returnErrors(rets: List[TypeRepr]) =
    rets.zipWithIndex
      .filterNot { (tpe, _) =>
        (tpe =:= TypeRepr.of[Int])
      }
      .map { (tpe, index) =>
        s"Return type at index ${index} is not an Int but ${tpe.show}"
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

  // Simple output function for testing
  def showLambdas(lambdas: List[ListLambda]): Expr[Unit] = {
    val prints =
      lambdas.map(lam =>
        '{
          println("The function " + ${ Expr(lam.name) } + " has arity " + ${
            Expr(lam.arity)
          } + ":")
          println(${ lam.lambda }(List.fill(${ Expr(lam.arity) })(0)))
        }
      )
    Expr.block(prints, '{})
  }

  code.asTerm match {
    case Inlined(_, _, Block(stats, _)) =>
      val lambdas =
        stats.map {
          case fun @ DefDef(name, params, returns, _) =>
            val args = params
              .flatMap {
                case TermParamClause(argsClause) => argsClause
                case _ =>
                  report.throwError(
                    s"Method $name has wrong parameters"
                  )
              }
            val rets = extractReturnTypes(returns.tpe)
            validate(name, args, rets)

            val inputArity = args.length
            val outputArity = rets.length

            val body: Term = fun.rhs.getOrElse {
              report.throwError(s"Method $name has no body")
            }

            ListLambda(
              inputArity,
              name,
              listifyMethod(args.map(_.symbol), body, outputArity)
            )
          case t: Term =>
            report.throwError("Expected def method, found " + t.show);
        }
      showLambdas(lambdas)
    case t: Term =>
      report.throwError("Expected statements, found " + t.show);
  }
}

inline def orka2(inline code: Unit): Unit = ${ orka2Impl('code) }
