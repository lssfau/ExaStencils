import exastencils.datastructures._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers.l4
import exastencils.parsers.l4.ParserL4

object Main {
  object CountingStrategy extends Strategy {
    override def apply : Boolean = {
      var constcount = 0
      var forcount = 0

      StateManager.apply(new Transformation({ case x : ConstantExpression => constcount += 1; WARN(x); Some(x) }))
      StateManager.apply(new Transformation({ case a : ForStatement => forcount += 1; Some(a) }))

      WARN(f"Counted $constcount consts and $forcount fors")

      true
    }
  }

  def main(args : Array[String]) : Unit = {
//    val newt = Duplicate(TreeManager.root)
//    // annotations copied?
//    println("original annotations")
//    TreeManager.root.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))
//    println("cloned annotations")
//    newt.getAnnotations.foreach(a => println(a.name + " <-> " + a.value))

//    DBG("previous:")
//    DBG(StateManager.root)
//    
//    var replacingStrategy = new Strategy
//    replacingStrategy += new Transformation({case x : ConstantExpression if(x.Value == 1) => Some(ConstantExpression(3)) })
//    replacingStrategy += new Transformation({case x : ConstantExpression if(x.Value.isInstanceOf[Int]) => Some(ConstantExpression(x.Value.asInstanceOf[Int] * 10)) })
//    replacingStrategy.apply
//    var absvarStrategy = new Strategy
//    absvarStrategy += new Transformation({ case x : Variable => Some(AbstractVariable("j", x.Type)) })
//    absvarStrategy.apply
//
//    DBG("===========================")
//    DBG("after:")
//    DBG(StateManager.root)
//
//    StateManager.apply(CountingStrategy)
    
    val p4 = new ParserL4
    val input4 = scala.io.Source.fromFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa").getLines.mkString
    DBG(input4)
    p4.parse(input4)
    //p4.parse()
  }
}