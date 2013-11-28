package exastencils.core

object Log {
  def error(s : Any) = {
    println(f"ERROR: $s")
    sys.exit(-1)
  }
  def warn(s : Any) = {
    println(f"WARN:  $s")
  }
  def debug(s : Any) = {
    println(f"DEBUG: $s")
  }
  def info(s : Any) = {
    println(f"INFO:  $s")
  }
}

object ERROR { def apply(s : Any) = Log.error(s) }
object WARN { def apply(s : Any) = Log.warn(s) }
object DBG { def apply(s : Any) = Log.debug(s) }
object INFO { def apply(s : Any) = Log.info(s) }
