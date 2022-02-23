package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.util.ir._

/// IR_FileAccess_Iostream
// helper functions for I/O operations using C++ STL I/O streams

trait IR_Iostream {

  def isAccessForWholeBlockAllowed(buf : IR_DataBuffer, conditionSpecified : Boolean) : IR_Expression = {
    conditionSpecified AndAnd // condition specified?
      !buf.fieldLayoutTransformed AndAnd // was field layout transformed?
      buf.accessWithoutExclusion AndAnd // is any layer excluded (e.g. ghost)?
      !(buf.numDimsData > buf.numDimsGrid) AndAnd // do we write a higher dim. datatype?
      !buf.accessPattern.isInstanceOf[IR_SWEAccessPattern] // compatible with access pattern?
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
      condition : IR_Expression) : IR_ScopedStatement = {

    val bytesAccessedKnownApriori = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known
    val tmpBuf = IR_VariableAccess("buffer", IR_SpecialDatatype(s"std::vector<${ buf.datatype.resolveBaseDatatype.prettyprint }>"))

    def loopOverDimsBuf(body : IR_Statement*) = buf.loopOverDims(condition, body : _*)

    new IR_IfCondition(isAccessForWholeBlockAllowed(buf, bytesAccessedKnownApriori),
      /* true: write whole buffer */
      ListBuffer(IR_PrintBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal)),
      /* false: write component by component in a loop */
      if (Knowledge.parIO_streams_useIntermediateBuffer) {
        // use manual buffering
        ListBuffer(
          IR_VariableDeclaration(tmpBuf),
          IR_MemberFunctionCall(tmpBuf, "reserve", buf.innerDimsLocalKJI.reduce(_ * _)),
          loopOverDimsBuf(
            buf.handleAccesses.flatten.map(acc =>
              IR_MemberFunctionCall(tmpBuf, "push_back", acc) : IR_Statement
            ) : _*),
          IR_PrintBlockBinary(
            stream,
            IR_AddressOf(IR_ArrayAccess(tmpBuf, 0)),
            IR_MemberFunctionCall(tmpBuf, "size") * buf.datatype.resolveBaseDatatype.typicalByteSize),
        )
      } else {
        // write each value via stream.write(...)
        ListBuffer(loopOverDimsBuf(IR_PrintBinary(stream, buf.handleAccesses.flatten)))
      })
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
      condition : IR_Expression) : IR_ScopedStatement = {

    val bytesAccessedKnownApriori = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known
    val tmpBuf = IR_VariableAccess("buffer", IR_SpecialDatatype(s"std::vector<${ buf.datatype.resolveBaseDatatype.prettyprint }>"))

    def loopOverDimsBuf(body : IR_Statement*) = buf.loopOverDims(condition, body : _*)

    new IR_IfCondition(isAccessForWholeBlockAllowed(buf, bytesAccessedKnownApriori),
      /* true: read whole buffer */
      ListBuffer(IR_ReadBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal)),
      /* false: read component by component in a loop */
      if (bytesAccessedKnownApriori && Knowledge.parIO_streams_useIntermediateBuffer) {
        // use manual buffering
        ListBuffer(
          IR_VariableDeclaration(tmpBuf),
          IR_MemberFunctionCall(tmpBuf, "reserve", buf.innerDimsLocalKJI.reduce(_ * _)),
          IR_ReadBlockBinary(stream, IR_AddressOf(IR_ArrayAccess(tmpBuf, 0)), buf.typicalByteSizeLocal),
          loopOverDimsBuf(
            buf.handleAccesses.flatten.zipWithIndex.map { case (acc, i) =>
              val offset = buf.referenceOffset - IR_ExpressionIndex(buf.beginIndices.toArray)
              val loopIdx = loopOverDimsBuf().indices.linearizeIndex(IR_LoopOverDimensions.defIt(buf.numDimsGrid) + offset)
              val idx = buf.handleAccesses.flatten.length * loopIdx
              IR_Assignment(acc, IR_ArrayAccess(tmpBuf, idx + i)) : IR_Statement
            } : _*)
        )
      } else {
        // read each value via stream.read(...)
        ListBuffer(loopOverDimsBuf(IR_ReadBinary(stream, buf.handleAccesses.flatten)))
      })
  }
}
