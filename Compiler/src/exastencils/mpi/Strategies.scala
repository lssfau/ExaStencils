package exastencils.mpi

import scala.collection.mutable.HashMap

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._

object RemoveMPIReferences extends DefaultStrategy("RemoveMPIReferences") {
  this += new Transformation("CleaningFunctions", {
    // FIXME: should delete node, currently not fully implemented -> QUICKFIX returns empty statements
    case _ : MPI_Barrier        => NullStatement
    case _ : MPI_Finalize       => NullStatement
    case _ : MPI_Init           => NullStatement
    case _ : MPI_SetRankAndSize => NullStatement

    case _ : MPI_IsRootProc     => BooleanConstant(true)
  })
}

object AddMPIDatatypes extends DefaultStrategy("AddMPIDatatypes") {
  var datatypes : HashMap[String, MPI_DataType] = HashMap()

  override def apply(node : Option[Node] = None) = {
    datatypes.clear
    super.apply(node)
  }

  this += new Transformation("Looking for data types", {
    case dt : MPI_DataType => {
      datatypes(dt.generateName) = dt
      dt
    }
  })

  this += new Transformation("Adding declaration and init code", {
    case globals : Globals =>
      for (dt <- datatypes)
        globals.variables += dt._2.generateDecl
      globals
    case func : FunctionStatement if ("initGlobals" == func.name) =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateCtor
      func
    case func : FunctionStatement if ("destroyGlobals" == func.name) =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateDtor
      func
  })
}
