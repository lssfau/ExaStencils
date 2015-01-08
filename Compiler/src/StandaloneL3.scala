import exastencils.core.StateManager
import exastencils.parsers.l3._
import exastencils.datastructures.l3._
import exastencils.prettyprinting.PpStream

object StandaloneL3 extends App {

  args.toList match {
    case List(inputFile) => compile(inputFile)
    case _ =>
      println("Exactly one command line argument expected: The input file name.")
  }

  def compile(inputFile: String) {

    val parserl3 = new ParserL3

    val scRoot = parserl3.parseFile(inputFile)
    StateManager.root_ = scRoot
    ValidationL3.apply

    val tcRoot = scRoot.progressToL4

    val printer = new PpStream()
    tcRoot.prettyprint(printer)

    println(printer.toString)
    println("Done!")
  }

}

