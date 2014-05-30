package exastencils.core

import scala.reflect.macros.blackbox

object Logger {
  import scala.reflect.macros._
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  // Level 1: Warn
  // Level 2: Debug
  // Level 4: Info

  protected var current : Int = 2
  def getLevel : Int = current
  def setLevel(level : Int) = { current = level }

  def error(s : AnyRef) = {
    sys.error("ERROR: " + s)
    sys.exit(-1) // just to be extra sure
  }

  def warn(s : AnyRef) : Unit = macro warnImpl
  def warning(s : AnyRef) : Unit = macro warnImpl
  def debug(s : AnyRef) : Unit = macro dbgImpl
  def dbg(s : AnyRef) : Unit = macro dbgImpl
  def info(s : AnyRef) : Unit = macro infoImpl

  def warnImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (exastencils.core.Logger.getLevel >= 1) {
        println("WARN:  " + $s)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def dbgImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (exastencils.core.Logger.getLevel >= 2) {
        println("DBG:   " + $s)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def infoImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (exastencils.core.Logger.getLevel >= 4) {
        println("INFO:  " + $s)
      }
    """
    }
    c.Expr[Unit](result)
  }
}
