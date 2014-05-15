package exastencils.core

import scala.reflect.macros._
import scala.reflect.runtime.universe._
import scala.language.experimental.macros

// This is just a wrapper for CompilerMacros.Logger, since macros need to be defined in a previous compilation run
object Logger {

  object Levels extends Enumeration {
    type Levels = Value
    val Error = Value(2)
    val Warn = Value(4)
    val Debug = Value(8)
    val Info = Value(16)
  }
  import Levels._

  protected val current : Levels = Debug

  def error(s : AnyRef) = {
    sys.error("ERROR: " + s)
    sys.exit(-1) // just to be extra sure
  }

  def warn(s : AnyRef) : Unit = examacros.Logger.warn(s)
  def warning(s : AnyRef) : Unit = examacros.Logger.warning(s)
  def dbg(s : AnyRef) : Unit = examacros.Logger.dbg(s)
  def debug(s : AnyRef) : Unit = examacros.Logger.debug(s)
  def info(s : AnyRef) : Unit = examacros.Logger.info(s)
}

