package exastencils.constraints

import scala.reflect.macros.blackbox
import exastencils.logger._

object Constraints {
  import scala.reflect.macros._
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  def getLevel : Int = Logger.getLevel
  def setLevel(level : Int) = { Logger.setLevel(level) }

  def updateValue(param : Any, value : Any) : Unit = macro updateValueImpl
  def condWarn(cond : Boolean, msg : AnyRef) : Unit = macro condWarnImpl
  def condError(cond : Boolean, msg : AnyRef) : Unit = macro condErrorImpl
  def condEnsureValue(param : Any, value : Any, cond : Boolean, msg : AnyRef) : Unit = macro condEnsureValueImpl
  def condEnsureValue(param : Any, value : Any, cond : Boolean) : Unit = macro condEnsureValueImpl2

  def updateValueImpl(c : blackbox.Context)(param : c.Expr[Any], value : c.Expr[Any]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($value != $param) {
        exastencils.logger.Logger.warn("Setting " + ${show(param.tree)} + " from " + $param + " to " + $value)
        if (exastencils.core.Settings.failOnConstraint)
          exastencils.logger.Logger.error("Exit on constraint was specified, shutting down now...")
        $param = $value
      }
    """
    }
    c.Expr[Unit](result)
  }

  def condWarnImpl(c : blackbox.Context)(cond : c.Expr[Boolean], msg : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($cond) {
        exastencils.logger.Logger.warn($msg)
        if (exastencils.core.Settings.failOnConstraint)
          exastencils.logger.Logger.error("Exit on constraint was specified, shutting down now...")
      }
    """
    }
    c.Expr[Unit](result)
  }

  def condErrorImpl(c : blackbox.Context)(cond : c.Expr[Boolean], msg : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($cond) {
        exastencils.logger.Logger.error($msg)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def condEnsureValueImpl(c : blackbox.Context)(param : c.Expr[Any], value : c.Expr[Any], cond : c.Expr[Boolean], msg : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      q"""if ($value != $param && ($cond)) {
        exastencils.logger.Logger.warn("Setting " + ${show(param.tree)} + " from " + $param + " to " + $value + " (" + $msg + ")")
        if (exastencils.core.Settings.failOnConstraint)
          exastencils.logger.Logger.error("Exit on constraint was specified, shutting down now...")
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
        exastencils.logger.Logger.warn("Setting " + ${show(param.tree)} + " from " + $param + " to " + $value + " (Constraint condition fullfilled: " + ${show(cond.tree)} + ")")
        if (exastencils.core.Settings.failOnConstraint)
          exastencils.logger.Logger.error("Exit on constraint was specified, shutting down now...")
        $param = $value
      }
    """
    }
    c.Expr[Unit](result)
  }
}
