package examacros

object Logger {

  import scala.reflect.macros._
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  def error(s : AnyRef) : Unit = macro errorImpl
  def warn(s : AnyRef) : Unit = macro warnImpl
  def warning(s : AnyRef) : Unit = macro warnImpl
  def debug(s : AnyRef) : Unit = macro dbgImpl
  def dbg(s : AnyRef) : Unit = macro dbgImpl
  def info(s : AnyRef) : Unit = macro infoImpl

  def errorImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit]({
      q"""
        sys.error("ERROR: " + $s)
        sys.exit(-1) // just to be extra sure
    """
    })
  }

  def warnImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (current.id >= Warn.id) {
        println("WARN:  " + $s)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def dbgImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (current.id >= Debug.id) {
        println("DBG:   " + $s)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def infoImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (current.id >= Info.id) {
        println("INFO:  " + $s)
      }
    """
    }
    c.Expr[Unit](result)
  }
}





