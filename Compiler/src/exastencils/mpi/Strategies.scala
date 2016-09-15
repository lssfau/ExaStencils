package exastencils.mpi

import scala.collection.mutable.HashMap

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.globals._

object RemoveMPIReferences extends DefaultStrategy("RemoveMPIReferences") {
  this += new Transformation("CleaningFunctions", {
    // TODO: think about replacing reduce, gather, etc. with copy operations
    case _ : MPI_Statement => List()

    case _ : MPI_IsRootProc => IR_BooleanConstant(true)
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
    case globals : Globals                                     =>
      for (dt <- datatypes)
        globals.variables += dt._2.generateDecl
      globals
    case func : IR_Function if ("initGlobals" == func.name)    =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateCtor
      func
    case func : IR_Function if ("destroyGlobals" == func.name) =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateDtor
      func
  })
}
