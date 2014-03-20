package harald.expert

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import harald.Impl._
import harald.ast.TreeL2
import harald.dsl._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.mpi._

object InitExternalFunctions extends Strategy("Init external functions") {
  this += new Transformation("Initing main function", {
    case tree : TreeL2 =>
      {
        var body : ListBuffer[Statement] = ListBuffer()

        for (c <- tree.Functions) {
          //println(c._2.name)
          if (c._2.name.equals("Application"))
            body += new StringLiteral(c._2.toString_cpp_body)
        }

        body += (new MPI_Finalize).cpp;

        tree.extfunctions += "Main" -> new ImplFunction("main", "int", ListBuffer(new ParameterInfo("argc", "int"), new ParameterInfo("argv", "char**")), body, Map(), "cpu")
      }
      Some(tree);
  });
}
