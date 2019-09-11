package exastencils.parallelization.api.cuda

import scala.collection._
import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.optimization.ir.IR_SimplifyExpression

/// CUDA_GatherFieldAccessLike

object CUDA_GatherFieldAccessLike extends QuietDefaultStrategy("Gather local FieldAccessLike nodes for shared memory") {
  var loopVariables = ListBuffer[String]()
  var fieldAccesses = new mutable.HashMap[String, List[IR_MultiDimFieldAccess]].withDefaultValue(Nil)
  var fieldIndicesConstantPart = new mutable.HashMap[String, List[Array[Long]]].withDefaultValue(Nil)
  var maximalFieldDim = Platform.hw_cuda_maxNumDimsBlock
  var writtenFields = ListBuffer[String]()

  def extractFieldIdentifier(access : IR_MultiDimFieldAccess) = {
    val field = access.field
    var identifier = field.codeName

    if (field.numSlots > 1) {
      access.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.slot.prettyprint }"
      }
    }

    identifier
  }

  this += new Transformation("Searching", {
    case stmt @ IR_Assignment(access : IR_MultiDimFieldAccess, _, _)                                =>
      writtenFields += extractFieldIdentifier(access)
      stmt
    case access : IR_MultiDimFieldAccess if access.field.fieldLayout.numDimsData <= maximalFieldDim =>
      val field = access.field
      val identifier = extractFieldIdentifier(access)

      // Evaluate indices. Should be of the form "variable + offset". Ignore all other fields.
      var suitableForSharedMemory = field.fieldLayout.numDimsData <= Platform.hw_cuda_maxNumDimsBlock
      val accessIndices = access.index.indices
      val indexConstantPart = Array.fill[Long](accessIndices.length)(0)

      accessIndices.indices.foreach(i => {
        accessIndices(i) match {
          case IR_Addition(ListBuffer(va @ IR_VariableAccess(name : String, _), IR_IntegerConstant(v : Long))) =>
            suitableForSharedMemory &= loopVariables.contains(name)
            indexConstantPart(i) = v
          case va @ IR_VariableAccess(name : String, _)                                                        =>
            suitableForSharedMemory &= loopVariables.contains(name)
          case IR_IntegerConstant(v : Long)                                                                    =>
            indexConstantPart(i) = v
          case _                                                                                               =>
            suitableForSharedMemory = false
        }
      })

      if (suitableForSharedMemory) {
        access.allowLinearization = true
        fieldAccesses(identifier) ::= access
        fieldIndicesConstantPart(identifier) ::= indexConstantPart
      }

      access
  }, true)
}

/// CUDA_ReplaceFieldAccessLike

object CUDA_ReplaceFieldAccessLike extends QuietDefaultStrategy("Replace local FieldAccessLike nodes for shared memory") {
  var fieldToOffset = ""
  var fieldOffset = IR_ExpressionIndex()
  var offsetForSharedMemoryAccess = 0L
  var sharedArrayStrides = Array[Long]()
  var executionDim = 0
  var baseIndex = IR_ExpressionIndex()
  var applySpatialBlocking = false

  def extractIdentifier(access : IR_MultiDimFieldAccess) = {
    val field = access.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.slot.prettyprint }"
      }
    }

    identifier
  }

  this += new Transformation("Searching", {
    case access : IR_MultiDimFieldAccess if fieldToOffset == extractIdentifier(access) =>
      val identifier = extractIdentifier(access)
      val deviation = (IR_ExpressionIndex(access.getAnnotation(CUDA_Kernel.ConstantIndexPart).get.asInstanceOf[Array[Long]]) - fieldOffset).indices

      if (applySpatialBlocking && deviation.take(executionDim).forall(x => IR_SimplifyExpression.evalIntegral(x) == 0)) {
        IR_SimplifyExpression.evalIntegral(deviation(executionDim)) match {
          // TODO: check if the datatypes in the next three lines are correct
          case 0                                                        => IR_VariableAccess("current", access.datatype)
          case x if 0L to offsetForSharedMemoryAccess contains x        => IR_VariableAccess("infront" + x, access.datatype)
          case y if 0L to -offsetForSharedMemoryAccess by -1 contains y => IR_VariableAccess("behind" + math.abs(y), access.datatype)
        }
      } else {
        new CUDA_SharedArrayAccess(IR_VariableAccess(CUDA_Kernel.KernelVariablePrefix + identifier, IR_PointerDatatype(access.field.resolveDeclType)), (access.index - fieldOffset).indices.take(executionDim).reverse, IR_ExpressionIndex(sharedArrayStrides))
      }
  })
}
