package exastencils.datastructures.ir.iv

import exastencils.core.StateManager
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals.Globals
import exastencils.knowledge.Knowledge
import exastencils.logger._

case class Timer(var name : Expression) extends UnduplicatedVariable {
  override def resolveName = s"timer_" + stripName
  override def resolveDataType = "StopWatch"

  def stripName = name.prettyprint.replaceAll("[^a-zA-Z0-9]", "_")

  override def getCtor() : Option[Statement] = {
    Some(AssignmentStatement(resolveName ~ ".timerName", StringConstant(stripName)))
  }
}

object VecShiftIndexStaticInit {
  val header = Knowledge.simd_header
  if (header != null)
    StateManager.findFirst[Globals].get.externalDependencies += header
}

case class VecShiftIndex(val offset : Int) extends UnduplicatedVariable {
  VecShiftIndexStaticInit // just to ensure VecShiftIndexStaticInit is initialized (once, since its an object)

  if (offset <= 0 || offset >= Knowledge.simd_vectorSize)
    Logger.error("VecShiftIndex out of bounds: " + offset)

  override def resolveName = "vShift" + offset
  override def resolveDataType = SpecialDatatype("__m512i")

  override def getCtor() : Option[Statement] = {
    val init = new StringLiteral(null : String)
    Knowledge.simd_instructionSet match {
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
