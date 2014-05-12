package logger

object Logger {

  import scala.reflect.macros._
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  object Levels extends Enumeration {
    type Levels = Value
    val Error = Value(2)
    val Warn = Value(4)
    val Debug = Value(8)
    val Info = Value(16)
  }
  import Levels._

  protected var current : Levels = Debug

  def setLevel(level : Levels) = { current = level }
  def getLevel() = current

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





