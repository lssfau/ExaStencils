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

package exastencils.logger

import scala.collection.mutable.Stack

/**
  * Logger object that currently writes to stdout.
  *
  * Because it uses Scala macros that need to be defined in a separate compilation run, this is a separate project.
  *
  */
object Logger {

  import scala.language.experimental.macros
  import scala.reflect.macros._

  // Level 1: Warn
  // Level 2: Debug
  // Level 4: Info

  /** The level for displaying only errors. */
  val ERROR = 0

  /** The level for displaying errors and warnings. */
  val WARNING = 1

  /** The level for displaying errors, warnings and debug messages. */
  val DEBUG = 2

  /** The level for displaying errors, warnings, debug messages and info messages. */
  val INFO = 4

  protected var current : Int = DEBUG
  var levelStack = Stack[Int]()

  /**
    * Returns the current logging level.
    *
    * @return The current logging level.
    */
  def getLevel : Int = current

  /**
    * Sets the current logging level.
    *
    * @param level A numerical value denoting the logging level, i.e., one of the LEVEL_* values defined in this entity.
    */
  def setLevel(level : Int) = { current = level }

  /**
    * Sets the current logging level while pushing the old one to an internal stack.
    *
    * @param level A numerical value denoting the logging level, i.e., one of the LEVEL_* values defined in this entity.
    */
  def pushLevel(level : Int) = {
    levelStack.push(current)
    current = level
  }

  /**
    * Sets the current logging level to the last logging level stored in the internal stack.
    */
  def popLevel() = {
    current = levelStack.pop()
  }

  /**
    * Outputs an error message and exits the program.
    *
    * @param s The message to display.
    */
  def error(s : AnyRef) : Nothing = macro errorImpl

  /**
    * Outputs a warning message.
    *
    * @param s The message to display.
    */
  def warn(s : AnyRef) : Unit = macro warnImpl

  /**
    * Outputs a warning message.
    *
    * @param s The message to display.
    */
  def warning(s : AnyRef) : Unit = macro warnImpl

  /**
    * Outputs a debug message.
    *
    * @param s The message to display.
    */
  def debug(s : AnyRef) : Unit = macro dbgImpl

  /**
    * Outputs a debug message.
    *
    * @param s The message to display.
    */
  def dbg(s : AnyRef) : Unit = macro dbgImpl

  /**
    * Outputs a info message.
    *
    * @param s The message to display.
    */
  def info(s : AnyRef) : Unit = macro infoImpl

  def isInNodeScope(c : blackbox.Context) = {
    var inNode = false

    var enclosing = c.internal.enclosingOwner
    while (!inNode && enclosing != c.universe.NoSymbol) {
      if (enclosing.isClass)
        if (enclosing.asClass.baseClasses.exists(_.fullName == "exastencils.datastructures.Node"))
          inNode = true

      enclosing = enclosing.owner
    }

    inNode
  }

  def errorImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Nothing] = {
    import c.universe._
    val result = {
      val fileName = Literal(Constant(c.enclosingPosition.source.file.file.getAbsolutePath))
      val line = Literal(Constant(c.enclosingPosition.line))

      def message = if (isInNodeScope(c)) q"$s.toString + location.toAppendString" else q"$s.toString"

      //q"""if (exastencils.logger.Logger.getLevel >= 0) {
      q"""{
        if (exastencils.config.Settings.produceHtmlLog) {
          exastencils.core.logger.Logger_HTML.printErr($fileName, $line, $message)
          exastencils.core.logger.Logger_HTML.finish
        }

        throw new exastencils.logger.LoggerError("ERROR: " + $message)
       }
    """
    }
    c.Expr[Nothing](result)
  }

  def warnImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      val fileName = Literal(Constant(c.enclosingPosition.source.file.file.getAbsolutePath))
      val line = Literal(Constant(c.enclosingPosition.line))

      def message = if (isInNodeScope(c)) q"$s.toString + location.toAppendString" else q"$s.toString"

      q"""if (exastencils.logger.Logger.getLevel >= 1) {
        println("WARN:  " + $message)
        if (exastencils.config.Settings.produceHtmlLog)
          exastencils.core.logger.Logger_HTML.printWarn($fileName, $line, $message)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def dbgImpl(c : whitebox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      val fileName = Literal(Constant(c.enclosingPosition.source.file.file.getAbsolutePath))
      val line = Literal(Constant(c.enclosingPosition.line))

      def message = if (isInNodeScope(c)) q"$s.toString + location.toAppendString" else q"$s.toString"

      q"""if (exastencils.logger.Logger.getLevel >= 2) {
        println("DBG:   " + $message)
        if (exastencils.config.Settings.produceHtmlLog)
          exastencils.core.logger.Logger_HTML.printDbg($fileName, $line, $message)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def infoImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      val fileName = Literal(Constant(c.enclosingPosition.source.file.file.getAbsolutePath))
      val line = Literal(Constant(c.enclosingPosition.line))

      def message = if (isInNodeScope(c)) q"$s.toString + location.toAppendString" else q"$s.toString"

      q"""if (exastencils.logger.Logger.getLevel >= 4) {
        println("INFO:  " + $message)
        if (exastencils.config.Settings.produceHtmlLog)
          exastencils.core.logger.Logger_HTML.printInfo($fileName, $line, $message)
      }
    """
    }
    c.Expr[Unit](result)
  }
}

class LoggerError(msg : String) extends RuntimeException(msg)
