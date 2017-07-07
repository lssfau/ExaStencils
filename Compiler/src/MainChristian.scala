import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4.{ L4_ResolveVariableAccesses, _ }
import exastencils.baseExt.ir._
import exastencils.core.StateManager
import exastencils.datastructures._
import exastencils.optimization.ir._
import exastencils.prettyprinting.PrettyPrintable

object MainChristian {

  case class MyRoot(var nodes : ListBuffer[Node]) extends RootNode {
    def progress = {
      nodes = nodes.map(_.asInstanceOf[L4_Progressable].progress)
    }
  }

  def main(args : Array[String]) : Unit = {
    exastencils.config.Knowledge.experimental_internalHighDimTypes = true
    exastencils.config.Knowledge.experimental_resolveInverseFunctionCall = "Runtime"

    //var tpdl = scala.xml.XML.loadFile("")
    val parser = exastencils.parsers.l4.L4_Parser
    val prog = "Function Application() : Unit { \n" +
      "//Var m : Matrix<Real, 2, 2>\n" +
      "//Var m2 : Matrix<Real, 2, 2>\n" +
      "//Var m : Matrix<Real, 2, 2> = {{1.0, 2.0},{1,1}} + {{2.0, 2.0}, {2.0, 2.0}} * {{2,2},{2,2}} - {{1,1},{1,1}}+{{2,2},{2,2}}\n" +
      "//Var n : ColumnVector<Real, 4> = [1; 2; 3; 4]\n" +
      "Var l : Matrix<Real, 5, 5> = [1 2 3 4 5; 6 7 8 9 0; 0 0 0 0 1; 1 1 1 1 1; 3 5 7 1 3]\n" +
      "Var m : Matrix<Real, 5, 5> = inverse(l)\n" +
      "//Var m : Real = inverse({{1.0, 2.0}, {3.0, 4.0}} + {{1.0, 2.0}, {3.0, 4.0}} )\n" +
      "//m2 = m\n" +
      "}" +
      "//Function Bla(blubb : Matrix<Real, 2, 2>) : Matrix<Real, 2, 2> {\n" +
      "//Var m : Matrix<Real, 2, 2> = {{1.0, 2.0}, {3.0, 4.0}}\n" +
      "//m = Bla(m)\n" +
      "//return(m)\n" +
      "//return {{2,2},{2,2}}\n" +
      "//}\n" +
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

    val ast = parser.parse(prog)
    val root = MyRoot(ListBuffer(ast))
    StateManager.setRoot(root)
    L4_ResolveVariableAccesses.apply()
    L4_ResolveDslFunctionReferences.apply()
    root.progress
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].baseName = ""
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].externalDependencies.clear()
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].internalDependencies.clear()
    System.out.println(root)
    IR_GeneralSimplify.doUntilDone(Some(root))

    IR_SetupMatrixExpressions.apply()
    IR_ExtractMatrices.apply()
    // repeat for new VariableAccesses TODO: generate the correct expressions directly
    //IR_SetupMatrixExpressions.apply()
    var sthChanged = true
    while (sthChanged) {
      // TODO: move matrix and vector specific parts of IR_GeneralSimplify to specialized strategy
      IR_GeneralSimplify.doUntilDone()

      IR_ResolveMatrixFunctions.apply()
      sthChanged = IR_ResolveMatrixFunctions.results.last._2.matches > 0
    }
    IR_ResolveMatrixAssignments.apply()
    IR_LinearizeMatrices.apply()
    //    HACK_IR_ResolveSpecialFunctionsAndConstants.apply()
    //    IR_GeneralSimplify.doUntilDone(Some(root))
    root.nodes(0).asInstanceOf[IR_Root].nodes(0).asInstanceOf[IR_UserFunctions].functions.foreach(x => x match {
      case y : PrettyPrintable => System.out.println(y.prettyprint)
      case _                   => System.out.println(x)
    })
  }
}
