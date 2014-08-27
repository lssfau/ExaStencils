package exastencils.core

import scala.reflect.macros.blackbox

object Constraints {
  import scala.reflect.macros._
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  def getLevel : Int = Logger.getLevel
  def setLevel(level : Int) = { Logger.setLevel(level) }

  def updateValue(param : Any, value : Any) = macro updateValueImpl
  def condWarn(cond : Boolean, msg : AnyRef) = macro condWarnImpl
  def condEnsureValue(param : Any, value : Any, cond : Boolean, msg : AnyRef) = macro condEnsureValueImpl
  def condEnsureValue(param : Any, value : Any, cond : Boolean) = macro condEnsureValueImpl2

  def updateValueImpl(c : blackbox.Context)(param : c.Expr[Any], value : c.Expr[Any]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($value != $param) {
        if (exastencils.core.Logger.getLevel >= 1) println("WARN:  " + "Setting " + ${show(param.tree)} + " from " + $param + " to " + $value)
        $param = $value
      }
    """
    }
    c.Expr[Unit](result)
  }

  def condWarnImpl(c : blackbox.Context)(cond : c.Expr[Boolean], msg : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if (exastencils.core.Logger.getLevel >= 1) {
        if ($cond) println("WARN:  " + $msg)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def condEnsureValueImpl(c : blackbox.Context)(param : c.Expr[Any], value : c.Expr[Any], cond : c.Expr[Boolean], msg : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($value != $param && ($cond)) {
        if (exastencils.core.Logger.getLevel >= 1) println("WARN:  " + "Setting " + ${show(param.tree)} + " from " + $param + " to " + $value + " (" + $msg + ")")
        $param = $value
      }
    """
    }
    c.Expr[Unit](result)
  }

  def condEnsureValueImpl2(c : blackbox.Context)(param : c.Expr[Any], value : c.Expr[Any], cond : c.Expr[Boolean]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($value != $param && ($cond)) {
        if (exastencils.core.Logger.getLevel >= 1) println("WARN:  " + "Setting " + ${show(param.tree)} + " from " + $param + " to " + $value + " (Constraint condition fullfilled: " + ${show(cond.tree)} + ")")
        $param = $value
      }
    """
    }
    c.Expr[Unit](result)
  }
}
