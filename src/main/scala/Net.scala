package orka
import scala.quoted.*

class OrkaNet[Q <: Quotes & Singleton](using val q: Q):
  import q.reflect.*

  def buildAdapter(fun: DefDef): Expr[Seq[Any] => Any] = {
    val paramTypes = fun.termParamss.flatMap(_.params).map(_.tpt.tpe.dealias)

    '{ (args: Seq[Any]) =>
      ${
        val argsTerm = '{ args }.asTerm
        val callArgs: List[Term] = paramTypes.zipWithIndex.map { case (pt, i) =>
          val elem = Apply(
            Select.unique(argsTerm, "apply"),
            List(Literal(IntConstant(i)))
          )
          pt.asType match {
            case '[t] => '{ ${ elem.asExprOf[Any] }.asInstanceOf[t] }.asTerm
          }
        }
        Apply(Ref(fun.symbol), callArgs).asExprOf[Any]
      }
    }
  }
