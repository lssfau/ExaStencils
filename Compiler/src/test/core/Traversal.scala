package test.core

import exastencils.core._
import exastencils.datastructures._
import exastencils.parsers.l4._

object Traversal {
  def main(args : Array[String]) : Unit = {

    var parserl4 = new ParserL4
    var x = parserl4.parseFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa")
    //    var x = parserl4.parseFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/src/harald/testmg/newDSL4.exa")
    println(x)

    StateManager.root_ = x

    val s = new Strategy("Printing Strategy") {
      val collector = new exastencils.core.collectors.StackCollector;

      override def apply(node : Option[Node] = None) = {
        StateManager.register(collector)
        super.apply(node)
        StateManager.unregister(collector)
      }

      this += new Transformation("Printing here...!", {
        case n : Any =>
          println(s"$n  --  ${collector.stack}")
          println(" ")
          Some(n)
      })
    }
    s.apply()
    sys.exit(0)
  }
}