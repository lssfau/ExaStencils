package exastencils.polyhedron

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.polyhedron.Isl.TypeAliases._

object LayoutTansformation extends DefaultStrategy("Layout Transformation") {

  import scala.language.implicitConversions

  implicit def long2val(l : Long) : isl.Val = isl.Val.intFromSi(Isl.ctx, l)

  private val tmpNamePrefix : String = "__ir_layout_toReplace_"
  private def tmpName(i : Int) : String = tmpNamePrefix + i
  private def tmpNameIdx(s : String) : Int = s.substring(tmpNamePrefix.length()).toInt

  private var trafo : isl.MultiAff = null

  private def getTrafo(gridDim : Int, dataDim : Int) : isl.MultiAff = {
    if (trafo != null)
      return trafo

    // perm must contain every integer between 0 and dataDim-1 exactly once
    var perm : Seq[Int] = 0 until dataDim
    if (Knowledge.opt_arrayOfFields) {
      perm = (gridDim until dataDim) ++ (0 until gridDim)
      trafo = createPermuteMAff(perm)
    }
    if (Knowledge.opt_useColorSplitting) {
      val cSplit = createColorSplittingMAff(dataDim, perm)
      // A.pullbackMultiAff(B) means B is applied before A, so pullback them in reverse order
      trafo = if (trafo == null) cSplit else cSplit.pullbackMultiAff(trafo)
    }

    // name dimensions (required by the AST generation later)
    for (i <- 0 until trafo.dim(T_IN))
      trafo = trafo.setDimName(T_IN, i, tmpName(i))
    trafo
  }

  private def createColorSplittingMAff(gridDim : Int, perm : Seq[Int]) : isl.MultiAff = {

    val gridIts = new Array[Int](gridDim)
    var min : Int = Int.MaxValue
    var n : Int = 0
    for ((p, i) <- perm.zipWithIndex)
      if (p < gridDim) {
        gridIts(n) = i
        min = math.min(min, i)
        n += 1
      }

    val dim = perm.size
    var trafo = isl.MultiAff.zero(isl.Space.alloc(Isl.ctx, 0, dim, dim + 1))
    val lSpace = isl.LocalSpace.fromSpace(trafo.getDomainSpace())
    for (i <- 0 until dim)
      trafo = trafo.setAff(i, isl.Aff.varOnDomain(lSpace, T_SET, i))

    // color index
    var aff = isl.Aff.zeroOnDomain(lSpace)
    for (i <- gridIts)
      aff = aff.setCoefficientSi(T_IN, i, 1)
    aff = aff.modVal(2L)
    trafo = trafo.setAff(dim, aff)

    // compressed access for innermost loop
    aff = isl.Aff.varOnDomain(lSpace, T_SET, min)
    aff = aff.sub(aff.modVal(2L))
    aff = aff.scaleDownUi(2)
    trafo = trafo.setAff(min, aff)

    trafo
  }

  private def createPermuteMAff(perm : Seq[Int]) : isl.MultiAff = {
    val dim = perm.size
    var trafo = isl.MultiAff.zero(isl.Space.alloc(Isl.ctx, 0, dim, dim))
    val lSpace = isl.LocalSpace.fromSpace(trafo.getDomainSpace())
    for ((p, i) <- perm.zipWithIndex) {
      val aff = isl.Aff.varOnDomain(lSpace, T_SET, p)
      trafo = trafo.setAff(i, aff)
    }
    trafo
  }

  private val processedLayouts = new IdentityHashMap[IR_FieldLayout, IR_Expression]()

  private def adaptLayout(layout : IR_FieldLayout) : (isl.MultiAff, Array[Long]) = {
    val ensure = { (b : Boolean, msg : String) =>
      if (!b) {
        Logger.warn(msg)
        return null
      }
    }
    var dim = layout.numDimsData
    ensure(dim == layout.layoutsPerDim.length, "Dimensions do not match! skipping layout")
    var domain = isl.Set.universe(isl.Space.setAlloc(Isl.ctx, 0, dim))
    for (i <- 0 until dim) {
      val total : Long =
        layout.layoutsPerDim(i).total match {
          case IR_IntegerConstant(c) if layout.defTotal(i) == c => c
          case _                                                =>
            Logger.warn(s"Cannot extract size of dimension $i, skipping access")
            return null
        }
      domain = domain.lowerBoundVal(T_SET, i, 0L)
      domain = domain.upperBoundVal(T_SET, i, total - 1)
    }
    val trafo : isl.MultiAff = getTrafo(layout.numDimsGrid, layout.numDimsData)
    domain = domain.apply(isl.Map.fromMultiAff(trafo))
    dim = domain.dim(T_SET)
    val extents = new Array[Long](dim)
    val newLayoutsPerDim = new Array[IR_FieldLayoutPerDim](extents.length)
    for (i <- 0 until dim) {
      var d = domain
      if (i < dim - 1)
        d = d.projectOut(T_SET, i + 1, dim - i - 1)
      d = d.projectOut(T_SET, 0, i)
      val min : isl.PwAff = d.dimMin(0)
      ensure(min.nPiece() == 1 && min.isCst(), "Min of transformed domain is not a constant: " + min)
      min.foreachPiece({
        (_, aff) =>
          val c = aff.getConstantVal()
          ensure(c.getDenSi() == 1, "Lower bound of transformed memory layout is not an integral number?! " + c)
          ensure(c.getNumSi() == 0, "Lower bound of transformed memory layout is not 0... " + c)
      })
      val max : isl.PwAff = d.dimMax(0)
      ensure(max.nPiece() == 1 && max.isCst(), "Max of transformed domain is not a constant: " + max)
      max.foreachPiece({
        (_, aff) =>
          val c = aff.getConstantVal()
          ensure(c.getDenSi() == 1, "upper bound of transformed memory layout is not an integral number?! " + c)
          val cl : Long = c.getNumSi()
          ensure(cl <= Int.MaxValue, "upper bound of transformed memory layout is larger than Int.MaxValue?! " + cl)
          extents(i) = c.getNumSi() + 1 // from 0 to c inclusive
          val flpd = IR_FieldLayoutPerDim(0, 0, 0, extents(i).toInt, 0, 0, 0)
          flpd.updateTotal()
          newLayoutsPerDim(i) = flpd
      })
    }
    layout.layoutsPerDim = newLayoutsPerDim
    layout.numDimsData = layout.layoutsPerDim.length // FIXME: why do I need to set this explicitly?!
    layout.referenceOffset = null // not valid anymore
    return (trafo, extents)
  }

  private def createASTforMultiAff(arg : (isl.MultiAff, Array[Long])) : IR_Expression = {
    if (arg == null) return null
    var maff : isl.MultiAff = arg._1
    val extents : Array[Long] = arg._2

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
    val args : Array[IR_Expression] = ASTExpressionBuilder.processArgs(expr)
    val indices = IR_ExpressionIndex(args.iterator.drop(1).toArray)
    val strides = IR_ExpressionIndex(extents.map(IR_IntegerConstant(_) : IR_Expression))
    return IR_Linearization.linearizeIndex(indices, strides)
  }

  this += new Transformation("transform", {
    case dfa : IR_DirectFieldAccess =>
      val layout = dfa.fieldSelection.fieldLayout
      val newIndex : IR_Expression =
        if (processedLayouts.containsKey(layout)) {
          processedLayouts.get(layout)
        } else {
          val trafoExtents = adaptLayout(layout)
          val exprs = createASTforMultiAff(trafoExtents)
          processedLayouts.put(layout, exprs)
          exprs
        }
      if (newIndex != null) {
        val wrappedIndex = IR_ExpressionStatement(Duplicate(newIndex))
        QuietDefaultStrategy("replace", new Transformation("now", {
          case IR_StringLiteral(id) if id.startsWith(tmpNamePrefix) => Duplicate(dfa.index(tmpNameIdx(id)))
          case IR_VariableAccess(id, _) if id.startsWith(tmpNamePrefix) => Duplicate(dfa.index(tmpNameIdx(id))) // HACK to deal with the HACK in IR_ExpressionIndex
        })).applyStandalone(wrappedIndex)
        IR_LinearizedFieldAccess(dfa.fieldSelection, IR_SimplifyExpression.simplifyIntegralExpr(wrappedIndex.expression))
      } else
        dfa
  })
}
