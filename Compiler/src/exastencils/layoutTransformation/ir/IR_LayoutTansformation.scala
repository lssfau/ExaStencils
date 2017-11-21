package exastencils.layoutTransformation.ir

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.communication.ir._
import exastencils.core.Duplicate
import exastencils.core.collectors.Collector
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.deprecated.ir.InitGeomCoords
import exastencils.field.ir._
import exastencils.interfacing.ir.IR_CopyFromExternalField
import exastencils.interfacing.ir.IR_CopyToExternalField
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.polyhedron.IR_ASTExpressionBuilder
import exastencils.polyhedron.IR_PolyOpt
import exastencils.polyhedron.Isl
import exastencils.polyhedron.Isl.TypeAliases._

object IR_LayoutTansformation extends CustomStrategy("Layout Transformation") {

  import scala.language.implicitConversions

  implicit def long2val(l : Long) : isl.Val = isl.Val.intFromSi(Isl.ctx, l)

  private val tmpNamePrefix : String = "__ir_layout_toReplace_"
  private def tmpName(i : Int) : String = tmpNamePrefix + i
  private def tmpNameIdx(s : String) : Int = s.substring(tmpNamePrefix.length()).toInt

  private def createIslTrafo(trafos : Seq[IR_GenericTransform]) : isl.MultiAff = {
    var trafoMaff : isl.MultiAff = null
    for (trafo <- trafos) {
      val maff = trafo.getIslTrafo()
      if (trafoMaff == null)
        trafoMaff = maff
      else
        trafoMaff = maff.pullbackMultiAff(trafoMaff)
    }
    trafoMaff
  }

  private def adaptLayout(layout : IR_FieldLayout, trafo : isl.MultiAff, fieldID : (String, Int)) : Unit = {
    val ensure : (Boolean, => String) => Unit = { (b, msg) =>
      if (!b)
        Logger.error(msg)
    }
    var dim = layout.numDimsData
    ensure(dim == trafo.dim(T_IN), s"Number of dimensions of layout transformation (${trafo.dim(T_IN)}) does not match actual layout dimensionality ($dim) for field $fieldID and trafo: $trafo")
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
          ensure(c.getNumSi() == 0, "Lower bound of transformed memory layout is not 0... " + c)
      })
      val max : isl.PwAff = d.dimMax(0)
      ensure(max.nPiece() == 1 && max.isCst(), "Max of transformed domain is not a constant: " + max)
      max.foreachPiece({ // there is only one
        (_, aff) =>
          val c = aff.getConstantVal()
          ensure(c.getDenSi() == 1, "upper bound of transformed memory layout is not an integral number?! " + c)
          val cl : Long = c.getNumSi()
          ensure(cl <= Int.MaxValue, "upper bound of transformed memory layout is greater or equal Int.MaxValue?! " + cl)
          val ext = c.getNumSi() + 1
          val flpd = IR_FieldLayoutPerDim(0, 0, 0, ext.toInt, 0, 0, 0)
          flpd.updateTotal()
          newLayoutsPerDim(i) = flpd
      })
    }
    layout.layoutsPerDim = newLayoutsPerDim
    layout.referenceOffset = null // field is not valid anymore
  }

  private def createASTforMultiAff(trafo : isl.MultiAff) : IR_ExpressionIndex = {
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
    return IR_ExpressionIndex(args.iterator.drop(1).toArray)
  }

  override def apply() : Unit = {
    this.transaction()
    Logger.info("Applying strategy " + name)

    val transformations = new mutable.HashMap[String, ArrayBuffer[IR_GenericTransform]]()
    val fieldConcs = new ArrayBuffer[IR_FieldConcatenation]()
    val fieldAliass = new ArrayBuffer[IR_ExternalFieldAlias]()

    // TODO: search for them here? or extract transformation statements earlier
    this.execute(new Transformation("collect transformation statements", {
      case trafo : IR_GenericTransform       =>
        for (field <- trafo.fields)
          transformations.getOrElseUpdate(field, ArrayBuffer[IR_GenericTransform]()) += trafo
        None
      case fieldConc : IR_FieldConcatenation =>
        fieldConcs += fieldConc
        None
      case alias : IR_ExternalFieldAlias     =>
        fieldAliass += alias
        None
    }))

    val processedLayouts = new IdentityHashMap[IR_FieldLayout, IR_ExpressionIndex]()
    val colCondColl = new ColorCondCollector()
    this.register(colCondColl)
    this.execute(new Transformation("transform", {
      case dfa : IR_DirectFieldAccess =>
        processDFA(dfa, transformations, processedLayouts, colCondColl)
        dfa
    }))

    val fieldReplace = new IdentityHashMap[IR_Field, (IR_Field, Int)]()
    for (fConc <- fieldConcs)
      fConc.addFieldReplacements(fieldReplace)

    for (alias <- fieldAliass)
      for (field <- IR_FieldCollection.getAllByIdentifier(alias.oldName))
        field.name = alias.newName

    if (!fieldReplace.isEmpty()) {
      colCondColl.reset()
      this.execute(new Transformation("concatenate and transform result", {
        case dfa : IR_DirectFieldAccess        =>
          for ((newField, index) <- Option(fieldReplace.get(dfa.fieldSelection.field))) {
            dfa.fieldSelection.field = newField
            val dim = dfa.index.indices.length + 1
            dfa.index.indices = java.util.Arrays.copyOf(dfa.index.indices, dim)
            dfa.index.indices(dim - 1) = IR_IntegerConstant(index)
            processDFA(dfa, transformations, processedLayouts, colCondColl)
          }
          dfa
        case node : IR_FieldSelection          =>
          for ((newField, _) <- Option(fieldReplace.get(node.field)))
            node.field = newField
          node
        case node : IR_IV_AbstractCommBuffer   =>
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
        case node : InitGeomCoords             =>
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
        case x : Product                       =>
          for (f <- x.productIterator)
            if (f.isInstanceOf[IR_Field])
              println("  der hatn field: " + x)
          x
      }))
    }
    this.unregister(colCondColl)

    this.commit()
  }

  def processDFA(dfa : IR_DirectFieldAccess, transformations : mutable.HashMap[String, ArrayBuffer[IR_GenericTransform]], processedLayouts : IdentityHashMap[IR_FieldLayout, IR_ExpressionIndex], colColl : ColorCondCollector) : Unit = {
    val fName = dfa.fieldSelection.field.name
    val layout = dfa.fieldSelection.fieldLayout
    var newIndex : IR_ExpressionIndex = null
    if (processedLayouts.containsKey(layout)) {
      newIndex = Duplicate(processedLayouts.get(layout))
    } else for (trafos <- transformations.get(fName)) {
      val trafoMaff : isl.MultiAff = createIslTrafo(trafos)
      val fLevel = dfa.fieldSelection.field.level
      adaptLayout(layout, trafoMaff, (fName, fLevel))
      val exprs : IR_ExpressionIndex = createASTforMultiAff(trafoMaff)
      processedLayouts.put(layout, exprs)
      newIndex = Duplicate(exprs)
    }
    if (newIndex != null) {
      QuietDefaultStrategy("replace", new Transformation("now", {
        case IR_StringLiteral(id) if id.startsWith(tmpNamePrefix)     => Duplicate.apply[IR_Expression](dfa.index(tmpNameIdx(id))) // IntelliJ workaround: specify IR_Expression explicitly
        case IR_VariableAccess(id, _) if id.startsWith(tmpNamePrefix) => Duplicate.apply[IR_Expression](dfa.index(tmpNameIdx(id))) // HACK to deal with the HACK in IR_ExpressionIndex
      })).applyStandalone(newIndex)
      val cSumMap : mutable.HashMap[IR_Expression, Long] = colColl.sum
      if (cSumMap != null) {
        QuietDefaultStrategy("simplify color", new Transformation("now", {
          case mod @ IR_Modulo(sum, IR_IntegerConstant(nrCol)) if colColl.nrCol == nrCol =>
            val sumMap : mutable.HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(sum)
            val sumCst : Long = sumMap.remove(IR_SimplifyExpression.constName).getOrElse(0L)
            if (sumMap == cSumMap)
              IR_IntegerConstant((sumCst + colColl.color) % nrCol)
            else
              mod
        })).applyStandalone(newIndex)
      }
      for (i <- 0 until newIndex.length)
        newIndex(i) = IR_SimplifyExpression.simplifyIntegralExpr(newIndex(i))
      dfa.index = newIndex
    }
  }
}

class ColorCondCollector extends Collector {

  private final val TMP_ANNOT : String = "CCCtmp"
  var sum : mutable.HashMap[IR_Expression, Long] = null
  var color : Long = -1
  var nrCol : Long = 0

  override def enter(node : Node) : Unit = {

    var cond : IR_Expression = null
    node match {
      case loop : IR_LoopOverDimensions if loop.condition.isDefined && loop.condition.get.isInstanceOf[IR_EqEq] =>
        cond = loop.condition.get
      case IR_IfCondition(c : IR_EqEq, _, fB) if fB.isEmpty                                                     =>
        cond = c
      case _                                                                                                    =>
        val annot : Option[Any] = node.getAnnotation(IR_PolyOpt.IMPL_CONDITION_ANNOT)
        if (annot.isDefined && annot.get.isInstanceOf[IR_EqEq])
          cond = annot.get.asInstanceOf[IR_Expression]
    }

    cond match {
      case IR_EqEq(IR_IntegerConstant(c), IR_Modulo(s, IR_IntegerConstant(nr))) =>
        sum = IR_SimplifyExpression.extractIntegralSum(s)
        color = c
        nrCol = nr
        node.annotate(TMP_ANNOT)
      case IR_EqEq(IR_Modulo(s, IR_IntegerConstant(nr)), IR_IntegerConstant(c)) =>
        sum = IR_SimplifyExpression.extractIntegralSum(s)
        color = c
        nrCol = nr
        node.annotate(TMP_ANNOT)
      case _                                                                    =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ =>
        if (node.removeAnnotation(TMP_ANNOT).isDefined)
          reset()
    }
  }

  override def reset() : Unit = {
    sum = null
    color = -1
    nrCol = 0
  }
}
