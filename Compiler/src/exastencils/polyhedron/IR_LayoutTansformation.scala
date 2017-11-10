package exastencils.polyhedron

import scala.collection.mutable

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.collectors.Collector
import exastencils.datastructures.CustomStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.polyhedron.Isl.TypeAliases._

object IR_LayoutTansformation extends CustomStrategy("Layout Transformation") {

  import scala.language.implicitConversions

  implicit def long2val(l : Long) : isl.Val = isl.Val.intFromSi(Isl.ctx, l)

  private val tmpNamePrefix : String = "__ir_layout_toReplace_"
  private def tmpName(i : Int) : String = tmpNamePrefix + i
  private def tmpNameIdx(s : String) : Int = s.substring(tmpNamePrefix.length()).toInt

  private def adaptLayout(layout : IR_FieldLayout, trafo : isl.MultiAff) : Unit = {
    val ensure = { (b : Boolean, msg : String) =>
      if (!b) {
        Logger.warn(msg)
        return
      }
    }
    var dim = layout.numDimsData
    var domain = isl.Set.universe(isl.Space.setAlloc(Isl.ctx, 0, dim))
    for (i <- 0 until dim) {
      val total : Long =
        layout.layoutsPerDim(i).total match {
          case IR_IntegerConstant(c) if layout.defTotal(i) == c => c
          case _                                                =>
            Logger.warn(s"Cannot extract size of dimension $i, skipping access")
            return
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

    val transformations = new mutable.HashMap[(String, Int), isl.MultiAff]()

    // TODO: search for them here? or extract transformation statements earlier
    this.execute(new Transformation("collect transformation statements", {
      case trafo : IR_GenericTransform =>
        val key = (trafo.fieldSelection.field.name, trafo.fieldSelection.level)
        val oldMaff = transformations.get(key)
        var maff : isl.MultiAff = trafo.getIslTrafo()
        if (oldMaff.isDefined)
          maff = maff.pullbackMultiAff(oldMaff.get)
        transformations(key) = maff
        trafo
    }))

    // name transformation dimensions (required by the AST generation later)
    transformations.transform {
      (_, t) =>
        var trafo = t
        for (i <- 0 until trafo.dim(T_IN))
          trafo = trafo.setDimName(T_IN, i, tmpName(i))
        trafo
    }

    val processedLayouts = new IdentityHashMap[IR_FieldLayout, IR_ExpressionIndex]()
    val colCondColl = new ColorCondCollector()
    this.register(colCondColl)
    this.execute(new Transformation("transform", {
      case dfa : IR_DirectFieldAccess =>
        val trafoKey = (dfa.fieldSelection.field.name, dfa.fieldSelection.field.level)
        val layout = dfa.fieldSelection.fieldLayout
        var newIndex : IR_ExpressionIndex = null
        if (processedLayouts.containsKey(layout)) {
          newIndex = Duplicate(processedLayouts.get(layout))
        } else for (trafo <- transformations.get(trafoKey)) {
          adaptLayout(layout, trafo)
          val exprs : IR_ExpressionIndex = createASTforMultiAff(trafo)
          processedLayouts.put(layout, exprs)
          newIndex = Duplicate(exprs)
        }
        if (newIndex != null) {
          QuietDefaultStrategy("replace", new Transformation("now", {
            case IR_StringLiteral(id) if id.startsWith(tmpNamePrefix)     => Duplicate.apply[IR_Expression](dfa.index(tmpNameIdx(id))) // IntelliJ workaround: specify IR_Expression explicitly
            case IR_VariableAccess(id, _) if id.startsWith(tmpNamePrefix) => Duplicate.apply[IR_Expression](dfa.index(tmpNameIdx(id))) // HACK to deal with the HACK in IR_ExpressionIndex
          })).applyStandalone(newIndex)
          val cSumMap : mutable.HashMap[IR_Expression, Long] = colCondColl.sum
          if (cSumMap != null) {
            QuietDefaultStrategy("simplify color", new Transformation("now", {
              case mod @ IR_Modulo(sum, IR_IntegerConstant(nrCol)) if colCondColl.nrCol == nrCol =>
                val sumMap : mutable.HashMap[IR_Expression, Long] = IR_SimplifyExpression.extractIntegralSum(sum)
                val sumCst : Long = sumMap.remove(IR_SimplifyExpression.constName).getOrElse(0L)
                if (sumMap == cSumMap)
                  IR_IntegerConstant((sumCst + colCondColl.color) % nrCol)
                else
                  mod
            })).applyStandalone(newIndex)
          }
          for (i <- 0 until newIndex.length)
            newIndex(i) = IR_SimplifyExpression.simplifyIntegralExpr(newIndex(i))
          dfa.index = newIndex
        }
        dfa
    }))
    this.unregister(colCondColl)

    this.commit()
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
