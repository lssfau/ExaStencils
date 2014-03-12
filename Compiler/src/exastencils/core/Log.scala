package exastencils.core

object Log {
  object Levels extends Enumeration {
    type Levels = Value
    val Error = Value(2)
    val Warn = Value(4)
    val Debug = Value(8)
    val Info = Value(16)
  }
  import Levels._

  var level : Levels = Warn

  def error(s : Any) = {
    sys.error(f"ERROR: $s")
    sys.exit(-1) // just to be extra sure
  }
  def warn(s : Any) = {
    if (level.id <= Warn.id) {
      println(f"WARN:  $s")
    }
  }
  def debug(s : Any) = {
    if (level.id <= Debug.id) {
      println(f"DEBUG: $s")
    }
  }
  def info(s : Any) = {
    if (level.id <= Info.id) {
      println(f"INFO:  $s")
    }
  }
}

object ERROR { def apply(s : Any) = Log.error(s) }
object WARN { def apply(s : Any) = Log.warn(s) }
object DBG { def apply(s : Any) = Log.debug(s) }
object INFO { def apply(s : Any) = Log.info(s) }
