import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers.l4
import exastencils.parsers.l4.ParserL4

import exastencils.knowledge._

import exastencils.application.GenerateCode_Application
import exastencils.domain.GenerateCode_Domain
import exastencils.multiGrid.GenerateCode_MultiGrid
import exastencils.primitives.GenerateCode_Primitives
import exastencils.util.GenerateCode_Util

object Main {
  object CountingStrategy extends Strategy("Counting") {
    override def apply : Boolean = {
      var constcount = 0
      var forcount = 0

//      StateManager.apply(new Transformation({ case x : Constant => constcount += 1; WARN(x); Some(x) }))
//      StateManager.apply(new Transformation({ case a : ForStatement => forcount += 1; Some(a) }))

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

//    val p4 = new ParserL4
//    val result = p4.parseFile("/scratch-local/schmittch/ExaStencils/ScalaExaStencil/Compiler/examples/level4_simple.exa")
//    
//    StateManager.root_ = result
//
//    
//    println(StateManager.root_)
//    var replacingStrategy = new Strategy("replacing")
//    replacingStrategy += new Transformation({ case x : BinaryExpression => Some(BinaryExpression("XXX", Constant(0), Constant(0))) })
////    replacingStrategy += new Transformation({ case x : BinaryExpression => throw TransformationException("omg") })
//    replacingStrategy.apply
//    
//    println(StateManager.root_)
//    
//    var positionStrategy = new Strategy("PositionStrategy")
//    positionStrategy += new Transformation({case x : Annotatable => x.getAnnotations.foreach(a => println(x + " " + a.name + " - " + a.value)); Some(x) })
//    //positionStrategy.apply

    
    Globals.printPath = s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated/";
    
    GenerateCode_Primitives();
    GenerateCode_MultiGrid();
    GenerateCode_Application();
    GenerateCode_Domain();
    GenerateCode_Util();
  }
}
