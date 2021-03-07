package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_BooleanConstant
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir.IR_VariableAccess
import exastencils.util.ir.IR_Print
import exastencils.util.ir.IR_PrintBinary
import exastencils.util.ir.IR_PrintBlockBinary
import exastencils.util.ir.IR_Read
import exastencils.util.ir.IR_ReadBinary
import exastencils.util.ir.IR_ReadBlockBinary

/// IR_FileAccess_Iostream
// helper functions for I/O operations using C++ STL I/O streams

trait IR_Iostream {

  def isAccessForWholeBlockAllowed(buf : IR_DataBuffer, conditionSpecified : Boolean, writeHighDimDatatypeInterleaved : Boolean) : IR_Expression = {
    conditionSpecified AndAnd // condition specified?
      buf.accessWithoutExclusion AndAnd // is any layer excluded (e.g. ghost)?
      !writeHighDimDatatypeInterleaved AndAnd // do we write higher dim. datatypes in an interleaved way?
      buf.accessPattern.isRegular // compatible with access pattern?
  }

  def printBufferAscii(
      buf : IR_DataBuffer,
      stream : IR_VariableAccess,
      condition : IR_Expression,
      separator : IR_Expression,
      optPrintComponents : Option[ListBuffer[IR_Expression]] = None,
      indent : Option[IR_Expression] = None) : IR_ScopedStatement = {

    val printComponents = optPrintComponents getOrElse ListBuffer[IR_Expression]()
    printComponents += "std::scientific"
    printComponents ++= indent
    printComponents ++= buf.handleAccesses.flatMap(accessesForPatternIndex =>
      accessesForPatternIndex.flatMap(acc => List(acc, separator)) :+ IR_Print.newline
    )
    buf.loopOverDims(condition, IR_Print(stream, printComponents))
  }

  def printBufferBinary(
      buf : IR_DataBuffer,
      stream : IR_VariableAccess,
      condition : IR_Expression,
      printInterleavedComponents : Boolean = true) : IR_ScopedStatement = {

    val bytesAccessedKnownApriori = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known
    val printAllComponentsPerLocation = printInterleavedComponents && buf.numDimsData > buf.numDimsGrid // determines if all components of a higher dim. dt are printed per grid node/cell/...

    IR_IfCondition(isAccessForWholeBlockAllowed(buf, bytesAccessedKnownApriori, printAllComponentsPerLocation),
      /* true: write whole buffer */
      IR_PrintBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal),
      /* false: write component by component in a loop */
      buf.loopOverDims(condition, IR_PrintBinary(stream, buf.handleAccesses.flatten)))
  }

  def readBufferAscii(
      buf : IR_DataBuffer,
      stream : IR_VariableAccess,
      condition : IR_Expression,
      skipSep : Option[IR_VariableAccess] = None) : IR_ScopedStatement = {

    // handle accesses of high dim datatypes
    val acc = buf.handleAccesses.flatMap(accessesForPatternIndex =>
      accessesForPatternIndex.flatMap(acc => List(acc) ++ skipSep)
    )

    buf.loopOverDims(condition, IR_Read(stream, acc : _*))
  }

  def readBufferBinary(
      buf : IR_DataBuffer,
      stream : IR_VariableAccess,
      condition : IR_Expression,
      printInterleavedComponents : Boolean = true) : IR_ScopedStatement = {

    val bytesAccessedKnownApriori = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known
    val printAllComponentsPerLocation = printInterleavedComponents && buf.numDimsData > buf.numDimsGrid // determines if all components of a higher dim. dt are printed per grid node/cell/...

    IR_IfCondition(isAccessForWholeBlockAllowed(buf, bytesAccessedKnownApriori, printAllComponentsPerLocation),
      /* true: read whole buffer */
      IR_ReadBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal),
      /* false: read component by component in a loop */
      buf.loopOverDims(condition, IR_ReadBinary(stream, buf.handleAccesses.flatten)))
  }
}
