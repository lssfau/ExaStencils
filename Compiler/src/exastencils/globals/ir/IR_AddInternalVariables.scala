//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.globals.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_IV_CommBuffer
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_IV_FieldData
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.ir.IR_ParallelizationInfo

/// IR_AddInternalVariables

// TODO: split and move to appropriate modules
object IR_AddInternalVariables extends DefaultStrategy("Add internal variables") {
  var declarationMap : HashMap[String, IR_VariableDeclaration] = HashMap()
  var ctorMap : HashMap[String, IR_Statement] = HashMap()
  var dtorMap : HashMap[String, IR_Statement] = HashMap()

  var bufferSizes : HashMap[String, IR_Expression] = HashMap()
  var bufferAllocs : HashMap[String, IR_Statement] = HashMap()
  var fieldAllocs : HashMap[String, IR_Statement] = HashMap()

  var deviceBufferSizes : HashMap[String, IR_Expression] = HashMap()
  var deviceBufferAllocs : HashMap[String, IR_Statement] = HashMap()
  var deviceFieldAllocs : HashMap[String, IR_Statement] = HashMap()

  var counter : Int = 0

  override def apply(node : Option[Node] = None) = {
    counter = 0
    for (map <- List(declarationMap, ctorMap, dtorMap, bufferSizes, bufferAllocs, fieldAllocs, deviceBufferSizes, deviceBufferAllocs, deviceFieldAllocs)) map.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    counter = 0
    for (map <- List(declarationMap, ctorMap, dtorMap, bufferSizes, bufferAllocs, fieldAllocs, deviceBufferSizes, deviceBufferAllocs, deviceFieldAllocs)) map.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collecting buffer sizes", {
    case buf : IR_IV_CommBuffer =>
      val id = buf.resolveAccess(buf.resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, buf.field.index, buf.field.level, buf.neighIdx).prettyprint
      if (!buf.field.layout.useFixedLayoutSizes) {
        if (bufferSizes.contains(id))
          bufferSizes(id).asInstanceOf[IR_Maximum].args += Duplicate(buf.size)
        else
          bufferSizes += (id -> IR_Maximum(ListBuffer(Duplicate(buf.size))))
      } else {
        val size = IR_SimplifyExpression.evalIntegral(buf.size)
        bufferSizes += (id -> (size max bufferSizes.getOrElse(id, IR_IntegerConstant(0)).asInstanceOf[IR_IntegerConstant].v))
      }
      buf

    case field : IR_IV_FieldData =>
      val cleanedField = Duplicate(field)
      cleanedField.slot = "slot"
      cleanedField.fragmentIdx = IR_LoopOverFragments.defIt

      val numDataPoints : IR_Expression = (0 until field.field.layout.numDimsData).map(dim => field.field.layout.idxById("TOT", dim)).reduceLeft(_ * _)
      var statements : ListBuffer[IR_Statement] = ListBuffer()

      val newFieldData = Duplicate(cleanedField)
      newFieldData.slot = if (field.field.numSlots > 1) "slot" else 0

      var innerStmts : ListBuffer[IR_Statement] =
        if (Knowledge.data_alignFieldPointers) {
          counter += 1
          ListBuffer(
            IR_VariableDeclaration(IR_SpecialDatatype("ptrdiff_t"), s"vs_$counter",
              Some(Platform.simd_vectorSize * IR_SizeOf(IR_RealDatatype))),
            IR_ArrayAllocation(newFieldData.basePtr, field.field.resolveDeclType, numDataPoints + Platform.simd_vectorSize - 1),
            IR_VariableDeclaration(IR_SpecialDatatype("ptrdiff_t"), s"offset_$counter",
              Some(((s"vs_$counter" - (IR_Cast(IR_SpecialDatatype("ptrdiff_t"), newFieldData.basePtr) Mod s"vs_$counter")) Mod s"vs_$counter") / IR_SizeOf(IR_RealDatatype))),
            IR_Assignment(newFieldData, newFieldData.basePtr + s"offset_$counter"))
        } else {
          ListBuffer(IR_ArrayAllocation(newFieldData, field.field.resolveDeclType, numDataPoints))
        }

      if (field.field.numSlots > 1)
        statements += new IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, "slot", 0),
          IR_Lower("slot", field.field.numSlots),
          IR_PreIncrement("slot"),
          innerStmts)
      else
        statements ++= innerStmts

      fieldAllocs += (cleanedField.prettyprint() -> IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.field.domain.index), statements),
        IR_ParallelizationInfo(potentiallyParallel = true)))

      field

    case buf : CUDA_BufferDeviceData =>
      val id = buf.resolveAccess(buf.resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, buf.field.index, buf.field.level, buf.neighIdx).prettyprint
      val size = IR_SimplifyExpression.evalIntegral(buf.size)

      deviceBufferSizes += (id -> (size max deviceBufferSizes.getOrElse(id, IR_IntegerConstant(0)).asInstanceOf[IR_IntegerConstant].v))

      buf

    case field : CUDA_FieldDeviceData =>
      val cleanedField = Duplicate(field)
      cleanedField.slot = "slot"
      cleanedField.fragmentIdx = IR_LoopOverFragments.defIt

      val numDataPoints : IR_Expression = (0 until field.field.layout.numDimsData).map(dim => field.field.layout.idxById("TOT", dim)).reduceLeft(_ * _)
      var statements : ListBuffer[IR_Statement] = ListBuffer()

      val newFieldData = Duplicate(cleanedField)
      newFieldData.slot = if (field.field.numSlots > 1) "slot" else 0

      var innerStmts = ListBuffer[IR_Statement](
        CUDA_Allocate(newFieldData, numDataPoints, field.field.resolveBaseDatatype))

      if (field.field.numSlots > 1)
        statements += new IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, "slot", 0),
          IR_Lower("slot", field.field.numSlots),
          IR_PreIncrement("slot"),
          innerStmts)
      else
        statements ++= innerStmts

      deviceFieldAllocs += (cleanedField.prettyprint() -> IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.field.domain.index), statements),
        IR_ParallelizationInfo(potentiallyParallel = true)))

      field

    case buf : CUDA_ReductionDeviceData =>
      val id = buf.resolveAccess(buf.resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression).prettyprint
      val totalSize : IR_Expression = buf.numPoints * buf.targetDt.getSizeArray.product
      if (Knowledge.data_genVariableFieldSizes) {
        if (deviceBufferSizes.contains(id))
          deviceBufferSizes(id).asInstanceOf[IR_Maximum].args += Duplicate(totalSize)
        else
          deviceBufferSizes += (id -> IR_Maximum(ListBuffer(Duplicate(totalSize))))
      } else {
        val size = IR_SimplifyExpression.evalIntegral(totalSize)
        deviceBufferSizes += (id -> (size max deviceBufferSizes.getOrElse(id, IR_IntegerConstant(0)).asInstanceOf[IR_IntegerConstant].v))
      }
      buf

    case buf : IR_IV_LoopCarriedCSBuffer =>
      val id = buf.resolveName()
      val size : IR_Expression =
        if (buf.dimSizes.isEmpty)
          IR_IntegerConstant(1)
        else
          Duplicate(buf.dimSizes.reduce(_ * _))
      bufferSizes.get(id) match {
        case Some(IR_Maximum(maxList)) => maxList += size
        case None                      => bufferSizes += (id -> IR_Maximum(ListBuffer(size)))
        case _                         => Logger.error("should not happen...")
      }
      buf
  })

  this += new Transformation("Updating temporary buffer allocations", {
    case buf : IR_IV_CommBuffer =>
      val id = buf.resolveAccess(buf.resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, buf.field.index, buf.field.level, buf.neighIdx).prettyprint
      val size = bufferSizes(id)

      if (Knowledge.data_alignTmpBufferPointers) {
        counter += 1
        bufferAllocs += (id -> IR_LoopOverFragments(ListBuffer[IR_Statement](
          IR_VariableDeclaration(IR_SpecialDatatype("ptrdiff_t"), s"vs_$counter",
            Some(Platform.simd_vectorSize * IR_SizeOf(IR_RealDatatype))),
          IR_ArrayAllocation(buf.basePtr, IR_RealDatatype, size + Platform.simd_vectorSize - 1),
          IR_VariableDeclaration(IR_SpecialDatatype("ptrdiff_t"), s"offset_$counter",
            Some(((s"vs_$counter" - (IR_Cast(IR_SpecialDatatype("ptrdiff_t"), buf.basePtr) Mod s"vs_$counter")) Mod s"vs_$counter") / IR_SizeOf(IR_RealDatatype))),
          IR_Assignment(buf, buf.basePtr + s"offset_$counter")),
          IR_ParallelizationInfo(potentiallyParallel = true)))
      } else {
        bufferAllocs += (id -> IR_LoopOverFragments(
          IR_ArrayAllocation(
            buf,
            if (buf.field.layout.datatype.isInstanceOf[IR_ComplexDatatype]) buf.field.layout.datatype
            else IR_RealDatatype,
            size
          ), IR_ParallelizationInfo(potentiallyParallel = true)))
      }

      buf

    case buf : CUDA_BufferDeviceData =>
      val id = buf.resolveAccess(buf.resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, buf.field.index, buf.field.level, buf.neighIdx).prettyprint
      val size = deviceBufferSizes(id)

      deviceBufferAllocs += (id -> IR_LoopOverFragments(CUDA_Allocate(buf, size, buf.field.resolveBaseDatatype), IR_ParallelizationInfo(potentiallyParallel = true)))

      buf

    case buf : CUDA_ReductionDeviceData =>
      val id = buf.resolveAccess(buf.resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression).prettyprint
      val size = deviceBufferSizes(id)

      deviceBufferAllocs += (id -> IR_LoopOverFragments(CUDA_Allocate(buf, size, buf.baseDt), IR_ParallelizationInfo(potentiallyParallel = true)))

      buf

    case buf : IR_IV_LoopCarriedCSBuffer =>
      val id = buf.resolveName()
      var size = bufferSizes(id)
      try {
        size = IR_SimplifyExpression.simplifyIntegralExpr(size)
      } catch {
        case _ : EvaluationException =>
      }
      if (Knowledge.data_alignFieldPointers) { // align this buffer iff field pointers are aligned
        counter += 1
        bufferAllocs += (id -> buf.wrapInLoops(IR_Scope(ListBuffer[IR_Statement](
          IR_VariableDeclaration(IR_SpecialDatatype("ptrdiff_t"), s"vs_$counter",
            Some(Platform.simd_vectorSize * IR_SizeOf(IR_RealDatatype))),
          IR_ArrayAllocation(buf.basePtr, IR_RealDatatype, size + Platform.simd_vectorSize - 1),
          IR_VariableDeclaration(IR_SpecialDatatype("ptrdiff_t"), s"offset_$counter",
            Some(((s"vs_$counter" - (IR_Cast(IR_SpecialDatatype("ptrdiff_t"), buf.basePtr) Mod s"vs_$counter")) Mod s"vs_$counter") / IR_SizeOf(IR_RealDatatype))),
          IR_Assignment(buf, buf.basePtr + s"offset_$counter")))))
      } else {
        bufferAllocs += (id -> buf.wrapInLoops(IR_ArrayAllocation(buf, buf.baseDatatype, size)))
      }
      buf
  })

  this += new Transformation("Extend SetupBuffers function", {
    case func : IR_Function if IR_AllocateDataFunction.fctName == func.name =>
      for (genericAlloc <- bufferAllocs.toSeq.sortBy(_._1) ++ fieldAllocs.toSeq.sortBy(_._1))
        if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
          func.body += IR_Scope(genericAlloc._2)
        else
          func.body += genericAlloc._2

      for (deviceAlloc <- deviceFieldAllocs.toSeq.sortBy(_._1) ++ deviceBufferAllocs.toSeq.sortBy(_._1))
        if ("Condition" == Knowledge.cuda_preferredExecution) {
          val loop = deviceAlloc._2.asInstanceOf[IR_LoopOverFragments]
          loop.body = ListBuffer(IR_IfCondition(IR_Negation(Knowledge.cuda_executionCondition), loop.body))
          func.body += loop
        } else if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
          func.body += IR_Scope(deviceAlloc._2)
        else
          func.body += deviceAlloc._2

      func
  })

  this += new Transformation("Collect", {
    case mem : IR_InternalVariable =>
      mem.registerIV(declarationMap, ctorMap, dtorMap)
      mem
  })

  this += new Transformation("Add to globals", {
    case globals : IR_GlobalCollection                       =>
      globals.variables ++= declarationMap.toSeq.sortBy(_._1).map(_._2)
      globals
    case func : IR_Function if "initGlobals" == func.name    =>
      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        func.body ++= ctorMap.toSeq.sortBy(_._1).map(s => IR_Scope(s._2))
      else
        func.body ++= ctorMap.toSeq.sortBy(_._1).map(_._2)
      func
    case func : IR_Function if "destroyGlobals" == func.name =>
      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        func.body ++= dtorMap.toSeq.sortBy(_._1).map(s => IR_Scope(s._2))
      else
        func.body ++= dtorMap.toSeq.sortBy(_._1).map(_._2)
      func
  })
}
