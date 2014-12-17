import exastencils.core.StateManager
import exastencils.parsers.l3._
import exastencils.datastructures.l3._

object StandaloneL3 extends App {

  val inputFile = args(0)

  var parserl3 = new ParserL3
  
  val root = parserl3.parseFile(inputFile)
  StateManager.root_ = root
  ValidationL3.apply

  val env = new Environment()
  
  println(root.toDc(env) )
  println("Done!")

}