package exastencils.datastructures.ir.iv

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.globals._
import exastencils.logger._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting._

object VecShiftIndexStaticInit {
  val header = Platform.simd_header
  if (header != null)
    StateManager.findFirst[Globals].get.externalDependencies += header
}

case class VecShiftIndex(val offset : Int) extends IR_UnduplicatedVariable {
  VecShiftIndexStaticInit // just to ensure VecShiftIndexStaticInit is initialized (once, since its an object)

  if (offset <= 0 || offset >= Platform.simd_vectorSize)
    Logger.error("VecShiftIndex out of bounds: " + offset)

  override def resolveName = "vShift" + offset
  override def resolveDatatype = IR_SpecialDatatype("__m512i")

  override def getCtor() : Option[IR_Statement] = {
    val init = new IR_StringLiteral(null : String)
    Platform.simd_instructionSet match {
      case "AVX512" =>
        if (Knowledge.useDblPrecision)
          init.value = "_mm512_set_epi64(" + (7 + offset to 0 + offset by -1).mkString(", ") + ')'
        else
          init.value = "_mm512_set_epi32(" + (15 + offset to 0 + offset by -1).mkString(", ") + ')'

      case "IMCI" =>
        val stride : Int = if (Knowledge.useDblPrecision) 2 else 1
        init.value = "_mm512_cvtfxpnt_round_adjustps_epi32((__m512) { " + (0 to 15).map(i => (i + offset * stride) % 16).mkString(", ") + " }, _MM_FROUND_TO_NEAREST_INT, _MM_EXPADJ_NONE)"

      case si => Logger.error("VecShiftIndex cannot be used for instruction set " + si)
    }

    return Some(IR_Assignment(IR_VariableAccess(resolveName, resolveDatatype), init))
  }
}

object LoopCarriedCSBuffer {
  final val commonPrefix = "_lcs"
}

abstract class AbstractLoopCarriedCSBuffer(private var identifier : Int, private val namePostfix : String,
    private val baseDatatype : IR_Datatype, private val freeInDtor : Boolean) extends IR_UnduplicatedVariable {

  override def getDeclaration() : IR_VariableDeclaration = {
    val superDecl = super.getDeclaration()
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1)
      superDecl.datatype = IR_ArrayDatatype(superDecl.datatype, Knowledge.omp_numThreads)
    return superDecl
  }

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1) {
      val begin = IR_VariableDeclaration(IR_IntegerDatatype, IR_LoopOverDimensions.threadIdxName, IR_IntegerConstant(0))
      val end = IR_LowerExpression(IR_VariableAccess(IR_LoopOverDimensions.threadIdxName, IR_IntegerDatatype), IR_IntegerConstant(Knowledge.omp_numThreads))
      val inc = IR_PreIncrementExpression(IR_VariableAccess(IR_LoopOverDimensions.threadIdxName, IR_IntegerDatatype))
      wrappedBody = new IR_ForLoop(begin, end, inc, ListBuffer(wrappedBody)) with OMP_PotentiallyParallel
    }
    return wrappedBody
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = baseAccess
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1)
      access = new IR_ArrayAccess(access, IR_StringLiteral("omp_get_thread_num()")) // access specific element of the outer "OMP-dim" first
    return super.resolveAccess(access, fragment, domain, field, level, neigh)
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << resolveAccess(resolveName, null, null, null, null, null)
  }

  override def resolveName() : String = {
    return LoopCarriedCSBuffer.commonPrefix + identifier + namePostfix
  }

  override def resolveDatatype() : IR_Datatype = {
    return new IR_PointerDatatype(baseDatatype)
  }

  override def resolveDefValue() : Option[IR_Expression] = {
    return Some(0)
  }

  override def getDtor() : Option[IR_Statement] = {
    val ptrExpr = resolveAccess(resolveName, null, null, null, null, null)
    if (freeInDtor)
      return Some(wrapInLoops(
        IR_IfCondition(ptrExpr,
          ListBuffer[IR_Statement](
            IR_ArrayFree(ptrExpr),
            new IR_Assignment(ptrExpr, 0)))))
    else
      return Some(wrapInLoops(new IR_Assignment(ptrExpr, 0)))
  }
}

case class LoopCarriedCSBuffer(val identifier : Int, val baseDatatype : IR_Datatype, val dimSizes : IR_ExpressionIndex)
  extends AbstractLoopCarriedCSBuffer(identifier, "", baseDatatype, !Knowledge.data_alignFieldPointers) {

  lazy val basePtr = new LoopCarriedCSBufferBasePtr(identifier, baseDatatype)

  override def registerIV(declarations : HashMap[String, IR_VariableDeclaration], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    super.registerIV(declarations, ctors, dtors)
    if (Knowledge.data_alignFieldPointers) // align this buffer iff field pointers are aligned -> register corresponding base pointer
      basePtr.registerIV(declarations, ctors, dtors)
  }
}

case class LoopCarriedCSBufferBasePtr(var identifier : Int, val baseDatatype : IR_Datatype)
  extends AbstractLoopCarriedCSBuffer(identifier, "_base", baseDatatype, true)
