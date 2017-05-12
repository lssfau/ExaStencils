package exastencils.prettyprinting

import exastencils.datastructures._
import exastencils.prettyprinting.PrintEnvironment.PrintEnvironment

/// FilePrettyPrintable

trait FilePrettyPrintable {
  def printToFile() : Unit
}

/// PrintToFile

object PrintToFile extends DefaultStrategy("Prettyprint all file-prettyprintable nodes") {
  this += new Transformation("Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile()
      printable
  })
}

/// PrintEnvironment

object PrintEnvironment extends Enumeration {
  type PrintEnvironment = Value
  val L1 = Value
  val L2 = Value
  val L3 = Value
  val L4 = Value
  val CPP = Value
  val CUDA = Value
}

/// PrettyPrintable

trait PrettyPrintable {
  def prettyprint(out : PpStream) : Unit

  final def prettyprint : String = prettyprint()
  // FIXME: HACK: allow usage of prettyprint without braces... -.-
  final def prettyprint(env : PrintEnvironment = PrintEnvironment.CPP) : String = {
    val out = new PpStream(env)
    prettyprint(out)
    out.toString()
  }
}
