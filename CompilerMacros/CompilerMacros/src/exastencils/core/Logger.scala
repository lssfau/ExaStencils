package exastencils.logger

/**
  * Logger object that currently writes to stdout.
  *
  * Because it uses Scala macros that need to be defined in a separate compilation run, this is a separate project.
  *
  */
object Logger {
  import scala.reflect.macros._
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  // Level 1: Warn
  // Level 2: Debug
  // Level 4: Info

  /** The level for displaying only errors. */
  val LEVEL_ERROR = 0

  /** The level for displaying errors and warnings. */
  val LEVEL_WARNING = 1

  /** The level for displaying errors, warnings and debug messages. */
  val LEVEL_DEBUG = 2

  /** The level for displaying errors, warnings, debug messages and info messages. */
  val LEVEL_INFO = 4

  protected var current : Int = LEVEL_DEBUG

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

  def errorImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Nothing] = {
    import c.universe._
    val result = {
      val fileName = Literal(Constant(c.enclosingPosition.source.file.file.getAbsolutePath))
      val line = Literal(Constant(c.enclosingPosition.line))

      //q"""if (exastencils.logger.Logger.getLevel >= 0) {
      q"""{
        sys.error("ERROR: " + $s)
        if (exastencils.core.Settings.produceHtmlLog) {
          Logger_HTML.printErr($fileName, $line, $s)
          Logger_HTML.finish
        }
        sys.exit(-1) // just to be extra sure
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

      q"""if (exastencils.logger.Logger.getLevel >= 1) {
        println("WARN:  " + $s)
        if (exastencils.core.Settings.produceHtmlLog)
          Logger_HTML.printWarn($fileName, $line, $s)
      }
    """
    }
    c.Expr[Unit](result)
  }

  def dbgImpl(c : blackbox.Context)(s : c.Expr[AnyRef]) : c.Expr[Unit] = {
    import c.universe._
    val result = {
      val fileName = Literal(Constant(c.enclosingPosition.source.file.file.getAbsolutePath))
      val line = Literal(Constant(c.enclosingPosition.line))

      q"""if (exastencils.logger.Logger.getLevel >= 2) {
        println("DBG:   " + $s)
        if (exastencils.core.Settings.produceHtmlLog)
          Logger_HTML.printDbg($fileName, $line, $s)
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

      q"""if (exastencils.logger.Logger.getLevel >= 4) {
        println("INFO:  " + $s)
        if (exastencils.core.Settings.produceHtmlLog)
          Logger_HTML.printInfo($fileName, $line, $s)
      }
    """
    }
    c.Expr[Unit](result)
  }
}
