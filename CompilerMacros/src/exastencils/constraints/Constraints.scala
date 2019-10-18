//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.constraints

import exastencils.logger._

object Constraints {

  import scala.language.experimental.macros
  import scala.reflect.macros._

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
        exastencils.logger.Logger.warn("Setting " + ${ show(param.tree) } + " from " + $param + " to " + $value)
        if (exastencils.config.Settings.failOnConstraint)
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
        if (exastencils.config.Settings.failOnConstraint)
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
        exastencils.logger.Logger.warn("Setting " + ${ show(param.tree) } + " from " + $param + " to " + $value + " (" + $msg + ")")
        if (exastencils.config.Settings.failOnConstraint)
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
        exastencils.logger.Logger.warn("Setting " + ${ show(param.tree) } + " from " + $param + " to " + $value + " (Constraint condition fullfilled: " + ${ show(cond.tree) } + ")")
        if (exastencils.config.Settings.failOnConstraint)
          exastencils.logger.Logger.error("Exit on constraint was specified, shutting down now...")
        $param = $value
      }
    """
    }
    c.Expr[Unit](result)
  }
}
