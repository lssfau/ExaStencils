package exastencils.datastructures.ir.iv

import exastencils.core._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting._

import scala.collection.mutable._

// FIXME: why is name an Expression?
case class Timer(var name : Expression) extends UnduplicatedVariable with Access {
  override def resolveName = s"timer_" + stripName
  override def resolveDataType = "StopWatch"

  def stripName = name.prettyprint.replaceAll("[^a-zA-Z0-9]", "_")

  override def getCtor() : Option[Statement] = {
    // FIXME: datatype for VariableAccess
    Some(AssignmentStatement(MemberAccess(VariableAccess(resolveName, Some(resolveDataType)), "timerName"), StringConstant(stripName)))
  }
}

object VecShiftIndexStaticInit {
  val header = Platform.simd_header
  if (header != null)
    StateManager.findFirst[Globals].get.externalDependencies += header
}

case class VecShiftIndex(val offset : Int) extends UnduplicatedVariable {
  VecShiftIndexStaticInit // just to ensure VecShiftIndexStaticInit is initialized (once, since its an object)

  if (offset <= 0 || offset >= Platform.simd_vectorSize)
    Logger.error("VecShiftIndex out of bounds: " + offset)

  override def resolveName = "vShift" + offset
  override def resolveDataType = SpecialDatatype("__m512i")

  override def getCtor() : Option[Statement] = {
    val init = new StringLiteral(null : String)
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

    return Some(AssignmentStatement(new VariableAccess(resolveName, resolveDataType), init))
  }
}

object LoopCarriedCSBuffer {
  final val commonPrefix = "_lcs"
}

case class LoopCarriedCSBuffer(var identifier : Int, val baseDatatype : Datatype, val dimSizes : MultiIndex) extends UnduplicatedVariable {

  override def getDeclaration() : VariableDeclarationStatement = {
    val superDecl = super.getDeclaration()
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1)
      superDecl.dataType = ArrayDatatype(superDecl.dataType, Knowledge.omp_numThreads)
    return superDecl
  }

  override def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1) {
      val begin = new VariableDeclarationStatement(IntegerDatatype, LoopOverDimensions.threadIdxName, IntegerConstant(0))
      val end = new LowerExpression(new VariableAccess(LoopOverDimensions.threadIdxName, IntegerDatatype), IntegerConstant(Knowledge.omp_numThreads))
      val inc = new PreIncrementExpression(new VariableAccess(LoopOverDimensions.threadIdxName, IntegerDatatype))
      wrappedBody = new ForLoopStatement(begin, end, inc, wrappedBody) with OMP_PotentiallyParallel
    }
    return wrappedBody
  }

  override def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    val access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    return resolveLCBAccess(access)
  }

  def resolveLCBAccess(baseAccess : Expression) : Expression = {
    var access = baseAccess
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1)
      access = new ArrayAccess(access, StringLiteral("omp_get_thread_num()"))
    return access
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << resolveAccess(resolveName, null, null, null, null, null)
  }

  override def resolveName() : String = {
    return LoopCarriedCSBuffer.commonPrefix + identifier
  }

  override def resolveDataType() : Datatype = {
    return new PointerDatatype(baseDatatype)
  }

  override def resolveDefValue() : Option[Expression] = {
    return Some(0)
  }

  override def getDtor() : Option[Statement] = {
    val ptrExpr = resolveAccess(resolveName, null, null, null, null, null)
    return Some(wrapInLoops(
      new ConditionStatement(ptrExpr,
        ListBuffer[Statement](
          FreeStatement(ptrExpr),
          new AssignmentStatement(ptrExpr, 0)))))
  }
}
