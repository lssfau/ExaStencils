import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.baseExt.l4.L4_MatrixExpression
import exastencils.datastructures._
import exastencils.core.StateManager
import exastencils.hack.ir.HACK_IR_ResolveSpecialFunctionsAndConstants
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.util.DuplicateNodes
import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ResolveVariableAccesses
import exastencils.prettyprinting.PrettyPrintable

object MainChristian {

  case class MyRoot(var nodes : ListBuffer[Node]) extends RootNode {
    def progress = {
      nodes = nodes.map(_.asInstanceOf[L4_Progressable].progress)
    }
  }

  def main(args : Array[String]) : Unit = {
    exastencils.config.Knowledge.experimental_internalHighDimTypes = true


    //var tpdl = scala.xml.XML.loadFile("")
    var parser = new exastencils.parsers.l4.ParserL4()
    var prog = "Function Application() : Unit { \n" +
      "Var m : Matrix<Real, 2, 2>\n"+
      "Var m2 : Matrix<Real, 2, 2>\n"+
      "//Var m : Matrix<Real, 2, 2> = {{1.0, 2.0}, {3.0, 4.0}} //+ {{2.0, 2.0}, {2.0, 2.0}} * {{2,2},{2,2}}\n" +
      "m2 = Bla(m) - {{1.0,1.0},{1.0,1.0}}\n" +
      "//Bla(m)\n" +
      "//Bla( {{1.0, 2.0}, {3.0, 4.0}})\n" +
      "//m2 = m\n" +
      "}" +
      "Function Bla(blubb : Matrix<Real, 2, 2>) : Matrix<Real, 2, 2> {\n" +
      "Var m : Matrix<Real, 2, 2> = {{1.0, 2.0}, {3.0, 4.0}}\n" +
      "m = Bla(m)\n" +
      "return(m)\n" +
      "//return {{2,2},{2,2}}\n" +
      "}\n" +
      "\n"

//    var prog = "Function Application() : Unit { \n" +
//      "Var m : Matrix<Real, 3, 3> = {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {7.0, 8.0, 2.0}}\n" +
//      "}\n"

//    var prog = "Function Application() : Unit { \n" +
//      "Var m : Matrix<Real, 4, 4> = {{1, 2, 3, 4}, {4, 5, 6, 6}, {7, 8, 2, 2}, {1, 1, 1, 1}}\n" +
//      "}\n"

//    var prog = "Function Application() : Unit { \n" +
////      "Var m : Matrix<Integer, 5, 5> = {{1.8, 2.0, 3.0, 4.0, 9.0}, {4.0, 5.0, 6.0, 6.0, 8.0}, {7.0, 8.0, 2.0, 2.0, 7.0}, {1.0, 3.0, 5.0, 7.0, 6.0}, {1.0, 2.0, 3.0, 4.0, 5.0}}\n" +
//      "Var m : Matrix<Real, 5, 5> = {{1, 2, 3, 4, 9}, {4, 5, 6, 6, 8}, {7, 8, 2, 2, 7}, {1, 3, 5, 7, 6}, {1, 2, 3, 4, 5}}\n" +
//    "}\n"


    var ast = parser.parse(prog)
    var root = MyRoot(ListBuffer(ast))
    StateManager.setRoot(root)
    L4_ResolveVariableAccesses.apply()
    L4_ResolveFunctionAccesses.apply()
    root.progress
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].baseName = ""
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].externalDependencies.clear()
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].internalDependencies.clear()
    System.out.println(root)
//    IR_GeneralSimplify.doUntilDone(Some(root))
    IR_ResolveMatrices.apply()
    IR_GeneralSimplify.doUntilDone(Some(root))
    IR_ResolveMatrices.apply()
    System.out.println(root)
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].functions.foreach(x => x match {
      case y : PrettyPrintable => System.out.println(y.prettyprint)
      case _ => System.out.println(x)
    })






/*

    var exp = IR_FunctionCall("inverse",  StateManager.findFirst[L4_MatrixExpression]().get.progress)
    //var exp = StateManager.findFirst[L4_MatrixExpression]().get.progress
    root.nodes.clear()
    root.nodes += exp
//    System.out.println(root)
//    var det = exastencils.hack.ir.HACK_IR_ResolveSpecialFunctionsAndConstants.calculateDeterminant(exp)
//    System.out.println(det)
//    var simpl = IR_SimplifyExpression.simplifyFloatingExpr(det)
//    System.out.println(simpl)

    HACK_IR_ResolveSpecialFunctionsAndConstants.apply(Some(root))
    System.out.println(root)
    var S = new DefaultStrategy("Mark")
    S += new Transformation("Mark", {
      case node : Node => if(node.hasAnnotation("blubbbi")) { Logger.error("doppelreferenz: " + node)} else { node.annotate("blubbbi") }; node
    })
    //S.apply()

    //new DuplicateNodes().apply()
    System.out.println(root)
    IR_GeneralSimplify.doUntilDone(Some(root))

    System.out.println(root)
*/

    /*
    System.out.println("")
    System.out.println("")
    System.out.println("")
    System.out.println("")

    var bla = IR_Multiplication(IR_Division(IR_RealConstant(1), IR_Subtraction(IR_Multiplication(IR_RealConstant(1), IR_RealConstant(4)), IR_Multiplication(IR_RealConstant(2), IR_RealConstant(3)))) , IR_RealConstant(4))
    System.out.println(bla)
    IR_GeneralSimplify.doUntilDone(Some(bla))
    System.out.println("bla: " + bla)
    */

  }
}
