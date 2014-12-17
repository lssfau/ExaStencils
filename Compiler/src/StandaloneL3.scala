import exastencils.core.StateManager
import exastencils.parsers.l3._
import exastencils.datastructures.l3._
import exastencils.prettyprinting.PpStream

object StandaloneL3 extends App {
  val inputFile = args(0)

  var parserl3 = new ParserL3

  val scRoot = parserl3.parseFile(inputFile)
  StateManager.root_ = scRoot
  ValidationL3.apply

  val tcRoot = scRoot.progressToL4

  val printer = new PpStream()
  tcRoot.prettyprint(printer)

  println(printer.toString)
  println("Done!")

}

