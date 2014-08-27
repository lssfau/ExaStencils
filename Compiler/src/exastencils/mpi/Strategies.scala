package exastencils.mpi

import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.util._

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
    case func : FunctionStatement if (("initGlobals" : Expression) == func.name) =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateCtor
      func
    case func : FunctionStatement if (("destroyGlobals" : Expression) == func.name) =>
      for (dt <- datatypes)
        func.body ++= dt._2.generateDtor
      func
  })
}

object AddInternalVariables extends DefaultStrategy("Adding internal variables") {
  var declarationMap : TreeMap[String, VariableDeclarationStatement] = TreeMap()
  var ctorMap : TreeMap[String, Statement] = TreeMap()
  var dtorMap : TreeMap[String, Statement] = TreeMap()

  this += new Transformation("Collecting", {
    case mem : iv.InternalVariable => // TODO: don't overwrite for performance reasons
      declarationMap += (mem.resolveName -> mem.getDeclaration)
      if (mem.getCtor().isDefined)
        ctorMap += (mem.resolveName -> mem.getCtor().get)
      if (mem.getDtor().isDefined)
        dtorMap += (mem.resolveName -> mem.getDtor().get)
      mem
  })

  this += new Transformation("Adding to globals", {
    case globals : Globals =>
      for (decl <- declarationMap)
        globals.variables += decl._2
      globals
    case func : FunctionStatement if (("initGlobals" : Expression) == func.name) =>
      func.body ++= ctorMap.map(_._2)
      func
    case func : FunctionStatement if (("destroyGlobals" : Expression) == func.name) =>
      func.body ++= dtorMap.map(_._2)
      func
  })

  var bufferSizes : TreeMap[Expression, Expression] = TreeMap()(Ordering.by(_.cpp))

  this += new Transformation("Collecting buffer sizes", {
    case buf : iv.TmpBuffer =>
      val id = buf.resolveAccess(buf.resolveName, LoopOverFragments.defIt, NullExpression, buf.field.index, buf.field.level, buf.neighIdx)
      if (Knowledge.comm_useLevelIndependentFcts) {
        if (bufferSizes.contains(id))
          bufferSizes.get(id).get.asInstanceOf[MaximumExpression].args += Duplicate(buf.size)
        else
          bufferSizes += (id -> MaximumExpression(ListBuffer(Duplicate(buf.size))))
      } else {
        val size = SimplifyExpression.evalIntegral(buf.size).toLong
        bufferSizes += (id -> (size max bufferSizes.getOrElse(id, IntegerConstant(0)).asInstanceOf[IntegerConstant].v))
      }
      buf
  })

  this += new Transformation("Extending SetupBuffers function", {
    // FIXME: this kind of matching is awkward, I want trafos that don't return nodes
    case func : FunctionStatement if (("setupBuffers" : Expression) == func.name) => {
      if (Knowledge.comm_useLevelIndependentFcts) {
        val s = new DefaultStrategy("Replacing level specifications")
        s += new Transformation("Search and replace", {
          case StringConstant("level")    => Knowledge.maxLevel : Expression
          case VariableAccess("level", _) => Knowledge.maxLevel : Expression
        })
        for (buf <- bufferSizes)
          s.applyStandalone(buf._2)
      }

      func.body += new LoopOverFragments(bufferSizes.map(buf => new AssignmentStatement(buf._1, "new double[" ~ buf._2 ~ "]") : Statement).to[ListBuffer]) with OMP_PotentiallyParallel
      func
    }
  })
}
