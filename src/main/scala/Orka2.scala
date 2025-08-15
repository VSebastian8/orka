package orka
import scala.quoted.*

def orka2Impl(code: Expr[Unit])(using Quotes): Expr[Unit] = {
  '{ println("ok") }
}

inline def orka2(inline code: Unit): Unit = ${ orka2Impl('code) }
