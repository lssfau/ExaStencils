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

package exastencils.layoutTransformation.ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.interfacing.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.polyhedron.Isl.TypeAliases._
import exastencils.polyhedron._
import exastencils.util.StackedMap

object IR_LayoutTansformation extends CustomStrategy("Layout Transformation") {

  private val DEBUG : Boolean = false

  import scala.language.implicitConversions

  implicit def long2val(l : Long) : isl.Val = isl.Val.intFromSi(Isl.ctx, l)

  private def addIdentityDims(mAff : isl.MultiAff, nr : Int) : isl.MultiAff = {
    var ma : isl.MultiAff = mAff
    val oldIn : Int = ma.dim(T_IN)
    val oldOut : Int = ma.dim(T_OUT)
    ma = ma.addDims(T_IN, nr) // .addDims(T_OUT, nr) not supported by isl -.-
    var id = isl.MultiAff.identity(isl.Space.alloc(Isl.ctx, 0, oldOut + nr, oldOut + nr))
    id = id.dropDims(T_IN, oldOut, nr)
    ma = id.pullbackMultiAff(ma)
    val lSpace = isl.LocalSpace.fromSpace(ma.getDomainSpace())
    for (i <- 0 until nr)
      ma = ma.setAff(oldOut + i, isl.Aff.varOnDomain(lSpace, T_SET, oldIn + i))
    ma
  }

  private def createIslTrafo(trafos : Seq[IR_GenericTransform], layout : IR_FieldLayout, fieldID : (String, Int)) : isl.MultiAff = {
    var matDims : Int = layout.numDimsData - layout.numDimsGrid
    var nrNonSvDims : Int = 0
    val svDims = ArrayBuffer[Int]()
    for (i <- layout.numDimsGrid until layout.numDimsData)
      if (layout.layoutsPerDim(i).isSingleValued())
        svDims += i
      else
        nrNonSvDims += 1
    var outDim : Int = 0
    var trafoMaff : isl.MultiAff = null
    for (trafo <- trafos) {
      val maff = trafo.getIslTrafo()
      if (trafoMaff == null)
        trafoMaff = maff
      else {
        // test if dimensionalities match, if not, try to adapt
        val dimDiff = trafo.inDim - outDim
        if (dimDiff != 0) {
          if (dimDiff == matDims || dimDiff == nrNonSvDims) {
            trafoMaff = addIdentityDims(trafoMaff, dimDiff)
            matDims = 0
            nrNonSvDims = 0
          } else
            Logger.error(s"Layout transformation (${ trafo.trafo.prettyprint() }) for field $fieldID is invalid: " +
              s"number of input field dimensions must be in ${ Set(outDim, outDim + matDims, outDim + nrNonSvDims) }, but is ${ trafo.inDim }.")
        }
        trafoMaff = maff.pullbackMultiAff(trafoMaff)
      }
      outDim = trafo.outDim
    }
    // add missing dimensions
    val inDims : Int = trafoMaff.dim(T_IN)
    if (inDims != layout.numDimsData) {
      if (inDims == layout.numDimsGrid)
        trafoMaff = addIdentityDims(trafoMaff, matDims)
      else if (inDims + svDims.length == layout.numDimsData)
        for (sv <- svDims)
          trafoMaff = trafoMaff.insertDims(T_IN, sv, 1)
      else
        Logger.error(s"Layout transformation for field $fieldID is invalid: " +
          s"number of input field dimensions for first transformation must be in " +
          s"${ Set(layout.numDimsGrid, layout.numDimsData - svDims.length, layout.numDimsData) }, but is $inDims.")
    }
    trafoMaff
  }

  private def adaptLayout(layout : IR_FieldLayout, trafo : isl.MultiAff, fieldID : (String, Int)) : Unit = {
    val ensure : (Boolean, => String) => Unit = { (b, msg) =>
      if (!b)
        Logger.error(msg)
    }
    var dim = layout.numDimsData
    ensure(dim == trafo.dim(T_IN), s"Number of dimensions of layout transformation (${ trafo.dim(T_IN) }) does not match actual layout dimensionality ($dim) for field $fieldID and trafo: $trafo")
    var domain = isl.Set.universe(isl.Space.setAlloc(Isl.ctx, 0, dim))
    for (i <- 0 until dim) {
      val total : Long =
        layout.layoutsPerDim(i).total match {
          case IR_IntegerConstant(c) if layout.defTotal(i) == c => c
          case _                                                =>
            Logger.error(s"Cannot extract size of dimension $i, skipping access")
        }
      domain = domain.lowerBoundVal(T_SET, i, 0L)
      domain = domain.upperBoundVal(T_SET, i, total - 1)
    }
    domain = domain.apply(isl.Map.fromMultiAff(trafo))
    dim = domain.dim(T_SET)
    val newLayoutsPerDim = new Array[IR_FieldLayoutPerDim](dim)
    for (i <- 0 until dim) {
      var d = domain
      if (i < dim - 1)
        d = d.projectOut(T_SET, i + 1, dim - i - 1)
      d = d.projectOut(T_SET, 0, i)
      val min : isl.PwAff = d.dimMin(0)
      ensure(min.nPiece() == 1 && min.isCst(), "Min of transformed domain is not a constant: " + min)
      min.foreachPiece({ // there is only one
        (_, aff) =>
          val c = aff.getConstantVal()
          ensure(c.getDenSi() == 1, "Lower bound of transformed memory layout is not an integral number?! " + c)
          ensure(c.getNumSi() >= 0, "Lower bound of transformed memory layout is negative... " + c)
      })
      val max : isl.PwAff = d.dimMax(0)
      ensure(max.nPiece() == 1 && max.isCst(), "Max of transformed domain is not a constant: " + max)
      max.foreachPiece({ // there is only one
        (_, aff) =>
          val c = aff.getConstantVal()
          ensure(c.getDenSi() == 1, "upper bound of transformed memory layout is not an integral number?! " + c)
          val cl : Long = c.getNumSi()
          ensure(cl < Int.MaxValue, "upper bound of transformed memory layout is greater or equal Int.MaxValue?! " + cl)
          var ext = c.getNumSi() + 1
          if (Knowledge.data_alignFieldPointers && i == 0) { // align extent of innermost dimension only
            val vsDec : Int = Platform.simd_vectorSize - 1
            ext = (ext + vsDec) & (~vsDec)
          }
          val flpd = IR_FieldLayoutPerDim(0, 0, 0, ext.toInt, 0, 0, 0)
          flpd.updateTotal()
          newLayoutsPerDim(i) = flpd
      })
    }
    layout.numDimsGrid = layout.numDimsData
    layout.datatype = layout.datatype.resolveBaseDatatype
    layout.layoutsPerDim = newLayoutsPerDim
    layout.referenceOffset = null // field is not valid anymore
  }

  private def createASTTemplateforMultiAff(trafo : isl.MultiAff) : (IR_ExpressionIndex => IR_ExpressionIndex) = {
    val tmpNamePrefix : String = "__LRep"

    def tmpName(i : Int) : String = tmpNamePrefix + i

    def tmpNameIdx(s : String) : Int = s.substring(tmpNamePrefix.length()).toInt

    var maff : isl.MultiAff = trafo

    val dim = maff.dim(T_IN)
    for (i <- 0 until dim)
      maff = maff.setDimName(T_IN, i, tmpName(i))

    maff = maff.moveDims(T_PAR, 0, T_IN, 0, dim)

    var pwmaff = isl.PwMultiAff.fromMultiAff(maff)
    pwmaff = pwmaff.projectDomainOnParams()

    // original array access must have been positive (this simplifies the generated access ASTs)
    var nonNeg = isl.Set.universe(pwmaff.getSpace().params())
    for (i <- 0 until dim)
      nonNeg = nonNeg.lowerBoundVal(T_PAR, i, 0L)
    pwmaff = pwmaff.intersectParams(nonNeg)

    val build = isl.AstBuild.alloc(pwmaff.getCtx())
    val expr : isl.AstExpr = build.accessFromPwMultiAff(pwmaff)
    val args : Array[IR_Expression] = IR_ASTExpressionBuilder.processArgs(expr)

    val expArr = args.iterator.drop(1).toArray
    val quietReplace = QuietDefaultStrategy("quiet")
    var oldInd : IR_ExpressionIndex = null
    quietReplace += new Transformation("replace", {
      case IR_StringLiteral(id) if id.startsWith(tmpNamePrefix)     => Duplicate.apply[IR_Expression](oldInd(tmpNameIdx(id))) // IntelliJ workaround: specify IR_Expression explicitly
      case IR_VariableAccess(id, _) if id.startsWith(tmpNamePrefix) => Duplicate.apply[IR_Expression](oldInd(tmpNameIdx(id))) // HACK to deal with the HACK in IR_ExpressionIndex
    })
    val template = {
      old : IR_ExpressionIndex =>
        oldInd = old
        val newAcc = IR_ExpressionIndex(Duplicate(expArr))
        quietReplace.applyStandalone(newAcc)
        newAcc
    }
    template
  }

  override def apply() : Unit = {
    this.transaction()
    Logger.info("Applying strategy " + name)

    val transformations = new HashMap[(String, Int), ArrayBuffer[IR_GenericTransform]]()
    val fieldConcs = new ArrayBuffer[IR_FieldConcatenation]()
    val fieldAliass = new ArrayBuffer[IR_ExternalFieldAlias]()

    // TODO: search for them here? or extract transformation statements earlier
    // collect layout trafo stmts
    this.execute(new Transformation("collect transformation statements", {
      case layColl : IR_LayoutTransformationCollection =>
        for (stmt <- layColl.trafoStmts) stmt match {
          case trafo : IR_GenericTransform       =>
            for (field <- trafo.fields)
              transformations.getOrElseUpdate(field, ArrayBuffer[IR_GenericTransform]()) += trafo
          case fieldConc : IR_FieldConcatenation =>
            fieldConcs += fieldConc
          case alias : IR_ExternalFieldAlias     =>
            fieldAliass += alias
        }
        None // consume whole collection
    }))

    // apply transform stmts (GenericTransform) (for the first time)
    val processedLayouts = new HashMap[(String, Int), IR_ExpressionIndex => IR_ExpressionIndex]()
    val colCondColl = new ColorCondCollector()
    if (!transformations.isEmpty) {
      this.register(colCondColl)
      this.execute(new Transformation("transform", {
        case dfa : IR_DirectFieldAccess =>
          processDFA(dfa, transformations, processedLayouts, colCondColl)
          dfa
      }))
    }

    // perform renameing (ExternalFieldAlias)
    for (alias <- fieldAliass)
      for (level <- alias.oldLevels)
        IR_FieldCollection.getByIdentifier(alias.oldName, level) match {
          case Some(field) => field.name = alias.newName
          case None        => Logger.error(s"field ${ alias.oldName }@$level should be renamed, but does not exist")
        }

    // create new (concatenated) fields (FieldConcatenation)
    val fieldReplace = new IdentityHashMap[IR_Field, (IR_Field, Int)]()
    for (fConc <- fieldConcs)
      fConc.addFieldReplacements(fieldReplace)

    // if there is something to replace...
    if (!fieldReplace.isEmpty()) {
      // ... perform replacement and apply transform stmts (Generic Transform) (again, since some may target the new, concatenated fields)
      colCondColl.reset()
      this.execute(new Transformation("concatenate and transform result", {
        case dfa : IR_DirectFieldAccess        =>
          for ((newField, index) <- Option(fieldReplace.get(dfa.field))) {
            dfa.field = newField
            val dim = dfa.index.indices.length + 1
            dfa.index.indices = java.util.Arrays.copyOf(dfa.index.indices, dim)
            dfa.index.indices(dim - 1) = IR_IntegerConstant(index)
            processDFA(dfa, transformations, processedLayouts, colCondColl)
          }
          dfa
        case node : IR_IV_AbstractCommBuffer    =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_IV_AbstractFieldData    =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_IV_FieldFlag            =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_IV_RemoteReqOutstanding =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_IV_LocalCommReady       =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_IV_LocalCommDone        =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_CopyFromExternalField   =>
          for ((newField, _) <- Option(fieldReplace.get(node.dest)))
            node.dest = newField
          node
        case node : IR_CopyToExternalField     =>
          for ((newField, _) <- Option(fieldReplace.get(node.src)))
            node.src = newField
          node
        case node : IR_IV_ActiveSlot           =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case x : Product                       =>
          if (DEBUG)
            for (f <- x.productIterator)
              if (f.isInstanceOf[IR_Field])
                Logger.error("unexpected IR_Field found in:  " + x)
          x
      }))
    }
    this.unregister(colCondColl)

    this.commit()
  }

  private def processDFA(dfa : IR_DirectFieldAccess, transformations : HashMap[(String, Int), ArrayBuffer[IR_GenericTransform]],
      processedLayouts : HashMap[(String, Int), IR_ExpressionIndex => IR_ExpressionIndex], colColl : ColorCondCollector) : Unit = {

    val fieldID : (String, Int) = (dfa.field.name, dfa.field.level)
    val trafosOpt = transformations.get(fieldID)
    if (trafosOpt.isEmpty)
      return

    var exprTemplate : IR_ExpressionIndex => IR_ExpressionIndex = processedLayouts.getOrElse(fieldID, null)
    if (exprTemplate == null) {
      val layout : IR_FieldLayout = dfa.field.layout
      val trafoMaff : isl.MultiAff = createIslTrafo(trafosOpt.get, layout, fieldID)
      adaptLayout(layout, trafoMaff, fieldID)
      exprTemplate = createASTTemplateforMultiAff(trafoMaff)
      processedLayouts.put(fieldID, exprTemplate)
    }

    val newIndex = exprTemplate(dfa.index)

    val colorInfo : StackedMap[HashMap[IR_Expression, Long], (Long, Long)] = colColl.colorInfo
    if (!colorInfo.isEmpty) {
      val qStrat = QuietDefaultStrategy("quiet")
      qStrat += new Transformation("simplify color", {
        case mod @ IR_Modulo(sum, IR_IntegerConstant(nrCol)) =>
          val sumMap : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(sum)
          val sumCst : Long = sumMap.remove(IR_SimplifyExpression.constName).getOrElse(0L)
          var trafoResult : IR_Expression = mod
          colorInfo.get(sumMap) match {
            case Some((color, nrColor)) if (nrCol == nrColor) =>
              trafoResult = IR_IntegerConstant(((sumCst + color) % nrCol + nrCol) % nrCol)
            case o                                            =>
              if (o.isDefined)
                Logger.warning(s"[layout trafo]  divisor of modulo operation in index ($nrCol) does not match the one in a surrounding condition (${ o.get._2 }) for expression  " + mod.prettyprint())
          }
          trafoResult
      })
      qStrat.applyStandalone(newIndex)
    }

    // simplifying here is very time intense, but may not be required... test
    // for (i <- 0 until newIndex.length)
    //   newIndex(i) = IR_SimplifyExpression.simplifyIntegralExpr(newIndex(i))
    dfa.index = newIndex
  }
}

class ColorCondCollector extends Collector {

  private final val TMP_ANNOT : String = "CCCtmp"
  val colorInfo = StackedMap[HashMap[IR_Expression, Long], (Long, Long)]()

  override def enter(node : Node) : Unit = {

    var cond : IR_Expression = null
    node match {
      case loop : IR_LoopOverDimensions if loop.condition.isDefined && loop.condition.get.isInstanceOf[IR_EqEq] =>
        cond = loop.condition.get
      case IR_IfCondition(c : IR_EqEq, _, fB) if fB.isEmpty                                                     =>
        cond = c
      case _                                                                                                    =>
        node.getAnnotation(IR_PolyOpt.IMPL_CONDITION_ANNOT) match {
          case Some(c : IR_Expression) =>
            cond = c
          case _                       =>
        }
    }

    if (cond != null) {
      colorInfo.push()
      processCondition(cond)
      node.annotate(TMP_ANNOT)
    }
  }

  private def processCondition(cond : IR_Expression) : Unit = {

    // prevent code duplication
    def storeColor(sumExpr : IR_Expression, color : Long, nrCol : Long) : Unit = {
      val sum : HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(sumExpr)
      var colorFixed = color
      for (cst <- sum.remove(IR_SimplifyExpression.constName)) // Option
        colorFixed = ((color - cst) % nrCol + nrCol) % nrCol
      colorInfo.put(sum, (colorFixed, nrCol))
    }

    cond match {
      case IR_EqEq(IR_IntegerConstant(c), IR_Modulo(s, IR_IntegerConstant(nr))) =>
        storeColor(s, c, nr)
      case IR_EqEq(IR_Modulo(s, IR_IntegerConstant(nr)), IR_IntegerConstant(c)) =>
        storeColor(s, c, nr)
      case IR_AndAnd(l, r)                                                      =>
        processCondition(l)
        processCondition(r)
      case _                                                                    =>
    }
  }

  override def leave(node : Node) : Unit = {
    if (node.removeAnnotation(TMP_ANNOT).isDefined)
      colorInfo.pop()
  }

  override def reset() : Unit = {
    colorInfo.clear()
  }
}
