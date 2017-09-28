import exastencils.base.l2._
import exastencils.baseExt.l2.L2_FieldIteratorAccess
import exastencils.config.Knowledge

object Testbed {
  def main(args : Array[String]) : Unit = {
    Knowledge.dimensionality = 2

    Knowledge.grid_isStaggered = true

    Knowledge.discr_hx :+= 10.0
    Knowledge.discr_hy :+= 10.0
    Knowledge.discr_hz :+= 10.0

    def it = L2_ExpressionIndex((0 until Knowledge.dimensionality).map(L2_FieldIteratorAccess(_) : L2_Expression).toArray)
/*
    {
      Logger.warn("Setting up stencil by hand")
      val sten = L2_Stencil("dummy", 0, 2, Array(1, 1), ListBuffer())

      sten.entries += L2_StencilOffsetEntry(L2_ConstIndex(-1, 0),
        -1.0 * L2_IntegrateOnGrid("integrateOverXStaggeredWestFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
          / L2_VirtualFieldAccess("vf_cellWidth_x", Some(L2_ConstIndex(-1, 0)))).asStencilMappingEntry
      sten.entries += L2_StencilOffsetEntry(L2_ConstIndex(1, 0),
        -1.0 * L2_IntegrateOnGrid("integrateOverXStaggeredEastFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
          / L2_VirtualFieldAccess("vf_cellWidth_x", 0, it + L2_ConstIndex(0, 0))).asStencilMappingEntry
      sten.entries += L2_StencilOffsetEntry(L2_ConstIndex(0, -1),
        -1.0 * L2_IntegrateOnGrid("integrateOverXStaggeredSouthFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
          / L2_VirtualFieldAccess("vf_stagCVWidth_y", 0, it + L2_ConstIndex(0, 0))).asStencilMappingEntry
      sten.entries += L2_StencilOffsetEntry(L2_ConstIndex(0, 1),
        -1.0 * L2_IntegrateOnGrid("integrateOverXStaggeredNorthFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
          / L2_VirtualFieldAccess("vf_stagCVWidth_y", 0, it + L2_ConstIndex(0, 1))).asStencilMappingEntry

      sten.entries += L2_StencilOffsetEntry(L2_ConstIndex(0, 0), L2_Negative(L2_Addition(sten.entries.map(_.coefficient)))).asStencilMappingEntry

      Logger.pushLevel(Logger.WARNING)
      sten.entries.foreach(e => Grid.applyStrategies(Some(e)))
      Logger.popLevel()

      sten.entries.transform(L2_GeneralSimplifyWrapper.process)

      Logger.warn(sten.printStencilToStr())
    }

    {
      Logger.warn("Setting up stencil from equation")
      import Helper._

      val layout = L2_FieldLayout("layout", 0, L2_RealDatatype, "face_x", Array(), 2, L2_ExpressionIndex(), false, false)
      val field = L2_Field("someField", 0, 0, L2_DomainFromAABB("global", L2_AABB(L2_ExpressionIndex(), L2_ExpressionIndex())), "someField_0", layout, 1, L2_NoBC)
      val fieldSel = L2_FieldSelection(field, 0, 0)

      var eq = L2_Equation(
        L2_Addition(

          // TODO: integrateOverXStaggeredWestFace ( nue * evalFlowAtXStaggeredWestFace ( u ) )

          L2_IntegrateOnGrid("integrateOverXStaggeredWestFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
            * (L2_FieldAccess(fieldSel, it)
            - L2_FieldAccess(fieldSel, it + L2_ConstIndex(-1, 0)))

            / (L2_VirtualFieldAccess("vf_nodePosition_x", 0, it + L2_ConstIndex(0, 0))
            - L2_VirtualFieldAccess("vf_nodePosition_x", 0, it + L2_ConstIndex(-1, 0))),

          L2_IntegrateOnGrid("integrateOverXStaggeredEastFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
            * (L2_FieldAccess(fieldSel, it)
            - L2_FieldAccess(fieldSel, it + L2_ConstIndex(1, 0)))

            / (L2_VirtualFieldAccess("vf_nodePosition_x", 0, it + L2_ConstIndex(1, 0))
            - L2_VirtualFieldAccess("vf_nodePosition_x", 0, it + L2_ConstIndex(0, 0))),

          L2_IntegrateOnGrid("integrateOverXStaggeredSouthFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
            * (L2_FieldAccess(fieldSel, it)
            - L2_FieldAccess(fieldSel, it + L2_ConstIndex(0, -1)))

            / (L2_VirtualFieldAccess("vf_cellCenter_y", 0, it + L2_ConstIndex(0, 0))
            - L2_VirtualFieldAccess("vf_cellCenter_y", 0, it + L2_ConstIndex(0, -1))),

          L2_IntegrateOnGrid("integrateOverXStaggeredNorthFace", 0, L2_VariableAccess("nue", L2_RealDatatype))
            * (L2_FieldAccess(fieldSel, it)
            - L2_FieldAccess(fieldSel, it + L2_ConstIndex(0, 1)))

            / (L2_VirtualFieldAccess("vf_cellCenter_y", 0, it + L2_ConstIndex(0, 1))
            - L2_VirtualFieldAccess("vf_cellCenter_y", 0, it + L2_ConstIndex(0, 0)))),
        0)

      Logger.pushLevel(Logger.WARNING)
      Grid.applyStrategies(Some(eq))
      Logger.popLevel()

      // extract stencil coefficients
      eq = L2_GeneralSimplifyWrapper.process(eq)

      targetField = L2_FieldAccess(fieldSel, it)
      unknowns.clear()

      val (a, f) = process(ListBuffer(eq))

      val sten = L2_Stencil("dummy", 0, 2, Array(1, 1), ListBuffer())

      for (i <- unknowns.indices) {
        val offset = (Duplicate(unknowns(i).index) - it).toConstIndex
        sten.entries += L2_StencilOffsetEntry(offset, a(0)(i)).asStencilMappingEntry
      }

      sten.entries.transform(L2_GeneralSimplifyWrapper.process)

      Logger.warn(sten.printStencilToStr())
    }

    {
      Logger.warn("Setting up convection stencil from equation")
      import Helper._

      val layout = L2_FieldLayout("layout", 0, L2_RealDatatype, "face_x", Array(), 2, L2_ExpressionIndex(), false, false)
      val field = L2_Field("someField", 0, 0, L2_DomainFromAABB("global", L2_AABB(L2_ExpressionIndex(), L2_ExpressionIndex())), "someField_0", layout, 1, L2_NoBC)
      val fieldSel = L2_FieldSelection(field, 0, 0)

      val layout_u = L2_FieldLayout("layout_u", 0, L2_RealDatatype, "face_x", Array(), 2, L2_ExpressionIndex(), false, false)
      val field_u = L2_Field("u", 0, 0, L2_DomainFromAABB("global", L2_AABB(L2_ExpressionIndex(), L2_ExpressionIndex())), "u_0", layout_u, 1, L2_NoBC)
      val fieldSel_u = L2_FieldSelection(field_u, 0, 0)
      def u = L2_FieldAccess(fieldSel_u, it)
      val layout_v = L2_FieldLayout("layout_v", 0, L2_RealDatatype, "face_y", Array(), 2, L2_ExpressionIndex(), false, false)
      val field_v = L2_Field("v", 0, 0, L2_DomainFromAABB("global", L2_AABB(L2_ExpressionIndex(), L2_ExpressionIndex())), "v_0", layout_v, 1, L2_NoBC)
      val fieldSel_v = L2_FieldSelection(field_v, 0, 0)
      def v = L2_FieldAccess(fieldSel_v, it)

      var eq = L2_Equation(

        L2_IntegrateOnGrid("integrateOverXStaggeredEastFace", 0, u * L2_FieldAccess(fieldSel, it))
          - L2_IntegrateOnGrid("integrateOverXStaggeredWestFace", 0, u * L2_FieldAccess(fieldSel, it))
          + L2_IntegrateOnGrid("integrateOverXStaggeredNorthFace", 0, v * L2_FieldAccess(fieldSel, it))
          - L2_IntegrateOnGrid("integrateOverXStaggeredSouthFace", 0, v * L2_FieldAccess(fieldSel, it))

        , 0)

      Logger.pushLevel(Logger.WARNING)
      Grid.applyStrategies(Some(eq))
      Logger.popLevel()

      // extract stencil coefficients
      eq = L2_GeneralSimplifyWrapper.process(eq)

      targetField = L2_FieldAccess(fieldSel, it)
      unknowns.clear()

      val (a, f) = process(ListBuffer(eq))

      val sten = L2_Stencil("dummy", 0, 2, Array(1, 1), ListBuffer())

      for (i <- unknowns.indices) {
        val offset = (Duplicate(unknowns(i).index) - it).toConstIndex
        sten.entries += L2_StencilOffsetEntry(offset, a(0)(i)).asStencilMappingEntry
      }

      sten.entries.transform(L2_GeneralSimplifyWrapper.process)

      Logger.warn(sten.printStencilToStr(true))
    }*/
  }
}

//object IndexMapping {
//  def apply(from : IR_Expression, to : IR_Expression) =
//    new IndexMapping(IR_ExpressionIndex(from), IR_ExpressionIndex(to))
//}
//
//case class IndexMapping(var from : IR_ExpressionIndex, var to : IR_ExpressionIndex) extends IR_Node {
//  def print : String = "[ " + from.prettyprint() + " -> " + to.prettyprint() + " ]"
//
//  def swap() : Unit = {
//    val tmp = to
//    to = from
//    from = tmp
//  }
//
//  def toIROffset() : IR_ExpressionIndex = {
//    val newOffset = Duplicate(to)
//
//    for (d <- 0 until from.length) {
//      IR_ReplaceVariableAccess.replace = Map(from.indices(d).prettyprint() -> 0) /*FIXME: use name of VA*/
//      IR_ReplaceVariableAccess.applyStandalone(newOffset)
//    }
//
//    newOffset.indices.transform(IR_SimplifyExpression.simplifyIntegralExpr)
//
//    while (newOffset.indices.length < 3) newOffset.indices :+= (0 : IR_Expression)
//
//    IR_GeneralSimplify.doUntilDoneStandalone(newOffset)
//
//    newOffset
//  }
//}
//
//case class StencilEntry(var index : IndexMapping, var coeff : IR_Expression) extends IR_Node {
//  def print : String = index.print + " => " + coeff.prettyprint()
//  def toIRStencilEntry() = IR_StencilOffsetEntry(index.toIROffset().toConstIndex, coeff)
//}
//
//case class MatFromStencil(var numDims : Int, var colStride : Array[Double], var entries : ListBuffer[StencilEntry]) extends IR_Node {
//  def print : String = entries.map(_.print).mkString(",\n")
//
//  def transpose() : MatFromStencil = {
//    val newStencil = Duplicate(this)
//    newStencil.colStride.transform(1.0 / _)
//
//    newStencil.entries.transform(entry => {
//      entry.index.swap()
//      for (d <- 0 until numDims) {
//        var done = false
//        while (!done) {
//          entry.index.from(d) match {
//            case _ : IR_FieldIteratorAccess =>
//              // TODO: more precise matching
//              done = true
//
//            case add : IR_Addition =>
//              val (iterator, remainder) = add.summands.partition(e => StateManager.findFirst[IR_FieldIteratorAccess]({ _ : IR_FieldIteratorAccess => true }, IR_ExpressionIndex(e)).isDefined)
//              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")
//
//              entry.index.from(d) = iterator.head
//              entry.index.to(d) = IR_Subtraction(entry.index.to(d), IR_Addition(remainder))
//
//            case sub : IR_Subtraction =>
//              entry.index.from(d) = IR_Addition(sub.left, IR_Negative(sub.right))
//
//            case mul : IR_Multiplication =>
//              val (iterator, remainder) = mul.factors.partition(e => StateManager.findFirst[IR_FieldIteratorAccess]({ _ : IR_FieldIteratorAccess => true }, IR_ExpressionIndex(e)).isDefined)
//              if (iterator.size != 1) Logger.error(s"unsupported: ${ iterator.size } != 1")
//
//              entry.index.from(d) = iterator.head
//              entry.index.to(d) = IR_Division(entry.index.to(d), IR_Multiplication(remainder))
//
//            case div : IR_Division =>
//              if (StateManager.findFirst[IR_FieldIteratorAccess]({ _ : IR_FieldIteratorAccess => true }, div.left).isDefined) {
//                entry.index.to(d) = IR_Multiplication(entry.index.to(d), div.right)
//                entry.index.from(d) = div.left
//              } else {
//                Logger.error("unsupported")
//              }
//
//            case other => Logger.error(other)
//          }
//        }
//      }
//      entry
//    })
//
//    IR_GeneralSimplify.doUntilDoneStandalone(newStencil)
//    newStencil
//  }
//
//  def add(other : MatFromStencil) : MatFromStencil = {
//    if (numDims != other.numDims) Logger.warn("Non-matching dimensionalities")
//    if ((0 until numDims).map(i => colStride(i) != other.colStride(i)).reduce(_ || _)) Logger.warn("Non-matching colStrides")
//
//    val newStencil = Duplicate(this)
//    newStencil.entries ++= Duplicate(other.entries)
//    newStencil.squash()
//
//    newStencil
//  }
//
//  def mul(other : MatFromStencil) : MatFromStencil = {
//    if (numDims != other.numDims) Logger.warn("Non-matching dimensionalities")
//    val newStencil = MatFromStencil(numDims, colStride.indices.map(i => colStride(i) * other.colStride(i)).toArray, ListBuffer())
//    for (left <- entries; right <- other.entries) {
//      // (x, y) * (y, z) // (left.from, left.to) * (right.from, right.to)
//      // wrap in ExpressionStatement to allow for matching of top-level accesses, eg when newTo is a single VariableAccess
//      val newTo = Duplicate(right.index.to)
//      for (d <- 0 until numDims) {
//        IR_ReplaceVariableAccess.replace = Map(right.index.from.indices(d).prettyprint() -> left.index.to.indices(d)) /*FIXME: use name of VA*/
//        IR_ReplaceVariableAccess.applyStandalone(newTo)
//      }
//
//      newTo.indices.transform(IR_SimplifyExpression.simplifyFloatingExpr)
//      newStencil.entries += StencilEntry(IndexMapping(Duplicate(left.index.from), newTo), left.coeff * right.coeff)
//    }
//    IR_GeneralSimplify.doUntilDone(Some(newStencil))
//    newStencil
//  }
//
//  def scale(factor : IR_Expression) : Unit = {
//    entries.foreach(_.coeff *= factor)
//  }
//
//  def kron(other : MatFromStencil) : MatFromStencil = {
//    val otherCloned = Duplicate(other)
//
//    object ShiftIteratorAccess extends DefaultStrategy("Replace something with something else") {
//      var baseDim : Int = 0
//
//      this += new Transformation("Search and replace", {
//        case it : IR_FieldIteratorAccess =>
//          if (it.dim < baseDim) it.dim += baseDim
//          it
//      }, false)
//    }
//
//    ShiftIteratorAccess.baseDim = numDims
//    ShiftIteratorAccess.applyStandalone(otherCloned)
//
//    val newStencil = MatFromStencil(numDims + otherCloned.numDims,
//      Duplicate(colStride ++ otherCloned.colStride),
//      entries.flatMap(l => otherCloned.entries.map(r =>
//        Duplicate(StencilEntry(IndexMapping(
//          IR_ExpressionIndex(l.index.from.indices ++ r.index.from.indices),
//          IR_ExpressionIndex(l.index.to.indices ++ r.index.to.indices)),
//          l.coeff * r.coeff)))))
//
//    IR_GeneralSimplify.doUntilDone(Some(newStencil))
//
//    newStencil
//  }
//
//  def squash() = {
//    val newEntries = HashMap[String, StencilEntry]()
//
//    for (entry <- entries) {
//      val id = entry.index.to.prettyprint()
//      if (newEntries.contains(id))
//        newEntries(id).coeff += entry.coeff
//      else
//        newEntries += ((id, entry))
//    }
//
//    entries = newEntries.toList.sortBy(_._1).map(_._2).to[ListBuffer]
//
//    IR_GeneralSimplify.doUntilDoneStandalone(this)
//  }
//
//  def assembleCases() : ListBuffer[ListBuffer[Int]] = {
//    def numCases(d : Int) : Int = if (colStride(d) >= 1) colStride(d).toInt else (1.0 / colStride(d)).toInt
//
//    var cases = ListBuffer.range(0, numCases(0)).map(i => ListBuffer(i))
//    for (d <- 1 until numDims)
//      cases = ListBuffer.range(0, numCases(d)).flatMap(i => cases.map(_ :+ i))
//
//    cases
//  }
//
//  def compileConditions(loopIt : IR_ExpressionIndex, stmts : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
//
//    object IR_ReplaceStencil extends QuietDefaultStrategy("Replace something with something else") {
//      var toReplace : MatFromStencil = MatFromStencil(0, Array(), ListBuffer())
//      var replacement : MatFromStencil = MatFromStencil(0, Array(), ListBuffer()) // to be overwritten
//
//      this += new Transformation("Search and replace", {
//        case sten : MatFromStencil if sten == toReplace => Duplicate(replacement)
//      }, false)
//    }
//
//    val numCases = (0 until numDims).map(d => if (colStride(d) >= 1) colStride(d).toInt else (1.0 / colStride(d)).toInt)
//    val cases = assembleCases()
//    cases.map(c => {
//      val redStencil = filterForSpecCase(c)
//      val redStmts = Duplicate(stmts)
//
////      Logger.warn(c.mkString("\t"))
////      Logger.warn(redStencil.print)
////      Logger.warn(redStencil.toIRStencil().printStencilToStr())
//
//      IR_ReplaceStencil.toReplace = this
//      IR_ReplaceStencil.replacement = redStencil
//      IR_ReplaceStencil.applyStandalone(redStmts)
//
//      IR_IfCondition(
//        (0 until numDims).map(d => IR_EqEq(c(d), IR_Modulo(loopIt.indices(d), numCases(d)))).reduce(IR_AndAnd),
//        redStmts) : IR_Statement
//    }
//    )
//  }
//
//  def filter() = {
//    entries = entries.filter(entry => {
//      // filter entries with zero coefficients
//      IR_SimplifyExpression.simplifyFloatingExpr(entry.coeff) match {
//        case IR_RealConstant(0.0) => false
//        case _                    => true
//      }
//    }).filter(entry => {
//      // filter entries with invalid indices
//      val indices = assembleCases().map(c => {
//        val mapTo = Duplicate(entry.index.to)
//        for (d <- 0 until numDims) {
//          IR_ReplaceVariableAccess.replace = Map(entry.index.from.indices(d).prettyprint() -> c(d)) /*FIXME: use name of VA*/
//          IR_ReplaceVariableAccess.applyStandalone(mapTo)
//        }
//        mapTo
//      }).flatMap(_.indices)
//
//      indices.map(IR_SimplifyExpression.simplifyFloatingExpr(_) match {
//        case IR_RealConstant(v) => v.isValidInt
//        case other              => Logger.warn(other); false
//      }).reduce(_ && _)
//    })
//  }
//
//  def filterForSpecCase(c : ListBuffer[Int]) : MatFromStencil = {
//    val newStencil = Duplicate(this)
//
//    newStencil.entries = newStencil.entries.filter(entry => {
//      // filter entries with invalid indices
//      for (d <- 0 until numDims) {
//        IR_ReplaceVariableAccess.replace = Map(entry.index.from.indices(d).prettyprint() -> c(d)) /*FIXME: use name of VA*/
//        IR_ReplaceVariableAccess.applyStandalone(entry.index.to)
//      }
//
//      entry.index.to.indices.map(IR_SimplifyExpression.simplifyFloatingExpr(_) match {
//        case IR_RealConstant(v) => v.isValidInt
//        case other              => Logger.warn(other); false
//      }).reduce(_ && _)
//    })
//
//    newStencil
//  }
//
//  def toIRStencil() = ??? // IR_Stencil("dummy_name", 0, entries.map(_.toIRStencilEntry()))
//}
//
//object Testbed {
//  def check1D() = {
//    def i = IR_FieldIteratorAccess(0)
//
//    val restrict = MatFromStencil(1, Array(2), ListBuffer(
//      StencilEntry(IndexMapping(i, 2 * i - 1), 1.0 / 4.0),
//      StencilEntry(IndexMapping(i, 2 * i + 0), 1.0 / 2.0),
//      StencilEntry(IndexMapping(i, 2 * i + 1), 1.0 / 4.0)
//    ))
//    Logger.warn("Restriction:\n" + restrict.print)
//
//    val op = MatFromStencil(1, Array(1), ListBuffer(
//      StencilEntry(IndexMapping(i, i - 1), 1.0),
//      StencilEntry(IndexMapping(i, i + 0), -2.0),
//      StencilEntry(IndexMapping(i, i + 1), 1.0)
//    ))
//    Logger.warn("Operator:\n" + op.print)
//
//    val factor = 2.0
//    val prolong = restrict.transpose()
//    prolong.scale(factor)
//    /*val prolong = MatFromStencil(ListBuffer(
//      StencilEntry(IndexMapping(i, (i + 1) / 2), factor / 4.0),
//      StencilEntry(IndexMapping(i, (i + 0) / 2), factor / 2.0),
//      StencilEntry(IndexMapping(i, (i - 1) / 2), factor / 4.0)
//    ))*/
//    Logger.warn("Prolongation:\n" + prolong.print)
//
//    var stencils = ListBuffer(restrict, op, prolong)
//
//    // enforce real constants
//    object ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
//      this += new Transformation("Search and replace", {
//        case IR_IntegerConstant(c) => IR_RealConstant(c)
//      })
//    }
//    object ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
//      this += new Transformation("Search and replace", {
//        case IR_RealConstant(c) => IR_IntegerConstant(c.toInt)
//      })
//    }
//    stencils.foreach(_.entries.foreach(entry => ReplaceIntWithReal.applyStandalone(entry.index)))
//
//    // calculate ra and rap
//    val ra = restrict.mul(op)
//    //Logger.warn("R * A:\n" + ra.print)
//    ra.squash()
//    //Logger.warn("R * A:\n" + ra.print)
//    ra.filter()
//    //Logger.warn("R * A:\n" + ra.print)
//    stencils += ra
//
//    val rap = ra.mul(prolong)
//    //Logger.warn("R * A * P:\n" + rap.print)
//    rap.squash()
//    //Logger.warn("R * A * P:\n" + rap.print)
//    rap.filter()
//    //Logger.warn("R * A * P:\n" + rap.print)
//    stencils += rap
//
//    stencils.foreach(_.entries.foreach(entry => ReplaceRealWithInt.applyStandalone(entry.index)))
//
//    Logger.warn("R * A:\n" + ra.print)
//    Logger.warn("R * A * P:\n" + rap.print)
//    //Logger.warn("R * A * P:\n" + rap.toIRStencil().printStencilToStr())
//
//    //Logger.warn(prolong.compileConditions(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(1)), ListBuffer()))
//  }
//
//  def check2D() = {
//    def i0 = IR_FieldIteratorAccess(0)
//    def i1 = IR_FieldIteratorAccess(1)
//    def i = IR_ExpressionIndex(i0, i1)
//
//    val linInterpolation = MatFromStencil(1, Array(2), ListBuffer(
//      StencilEntry(IndexMapping(i0, 2 * i0 - 1), 1.0 / 4.0),
//      StencilEntry(IndexMapping(i0, 2 * i0 + 0), 1.0 / 2.0),
//      StencilEntry(IndexMapping(i0, 2 * i0 + 1), 1.0 / 4.0)))
//
//    val restrict = linInterpolation.kron(linInterpolation)
//
//    /*val restrict = MatFromStencil(1, Array(2), ListBuffer(
//      StencilEntry(IndexMapping(i0, 2 * i0 - 1), 1.0 / 4.0),
//      StencilEntry(IndexMapping(i0, 2 * i0 + 0), 1.0 / 2.0),
//      StencilEntry(IndexMapping(i0, 2 * i0 + 1), 1.0 / 4.0))).kron(
//      MatFromStencil(1, Array(2), ListBuffer(
//        StencilEntry(IndexMapping(i1, 2 * i1 - 1), 1.0 / 4.0),
//        StencilEntry(IndexMapping(i1, 2 * i1 + 0), 1.0 / 2.0),
//        StencilEntry(IndexMapping(i1, 2 * i1 + 1), 1.0 / 4.0))))*/
//
//    /*val restrict = MatFromStencil(2, Array(2, 2), ListBuffer(
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 - 1, 2 * i1 - 1)), 1.0 / 16.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 - 1, 2 * i1 + 0)), 1.0 / 8.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 - 1, 2 * i1 + 1)), 1.0 / 16.0),
//
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 + 0, 2 * i1 - 1)), 1.0 / 8.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 + 0, 2 * i1 + 0)), 1.0 / 4.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 + 0, 2 * i1 + 1)), 1.0 / 8.0),
//
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 + 1, 2 * i1 - 1)), 1.0 / 16.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 + 1, 2 * i1 + 0)), 1.0 / 8.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(2 * i0 + 1, 2 * i1 + 1)), 1.0 / 16.0)
//    ))*/
//    Logger.warn("Restriction:\n" + restrict.print)
//
//    val op = MatFromStencil(2, Array(1, 1), ListBuffer(
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(i0 - 1, i1 + 0)), 1.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(i0 + 0, i1 - 1)), 1.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(i0 + 0, i1 + 0)), -4.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(i0 + 1, i1 + 0)), 1.0),
//      StencilEntry(IndexMapping(i, IR_ExpressionIndex(i0 + 0, i1 + 1)), 1.0)
//    ))
//    Logger.warn("Operator:\n" + op.print)
//
//    val factor = 1.0 * 4.0
//    val prolong = restrict.transpose()
//    prolong.scale(factor)
//    /*val prolong = MatFromStencil(ListBuffer(
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 + 1) / 2, (i1 + 1) / 2)), factor / 16.0),
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 + 1) / 2, (i1 + 0) / 2)), factor / 8.0),
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 + 1) / 2, (i1 - 1) / 2)), factor / 16.0),
//
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 + 0) / 2, (i1 + 1) / 2)), factor / 8.0),
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 + 0) / 2, (i1 + 0) / 2)), factor / 4.0),
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 + 0) / 2, (i1 - 1) / 2)), factor / 8.0),
//
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 - 1) / 2, (i1 + 1) / 2)), factor / 16.0),
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 - 1) / 2, (i1 + 0) / 2)), factor / 8.0),
//      StencilEntry(IndexMapping(2, i, IR_ExpressionIndex((i0 - 1) / 2, (i1 - 1) / 2)), factor / 16.0)
//    ))*/
//    Logger.warn("Prolongation:\n" + prolong.print)
//
//    var stencils = ListBuffer(restrict, op, prolong)
//
//    // enforce real constants
//    object ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
//      this += new Transformation("Search and replace", {
//        case IR_IntegerConstant(c) => IR_RealConstant(c)
//      })
//    }
//    object ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
//      this += new Transformation("Search and replace", {
//        case IR_RealConstant(c) => IR_IntegerConstant(c.toInt)
//      })
//    }
//    stencils.foreach(_.entries.foreach(entry => ReplaceIntWithReal.applyStandalone(entry.index)))
//
//    // calculate ra and rap
//    val ra = restrict.mul(op)
//    //Logger.warn("R * A:\n" + ra.print)
//    ra.squash()
//    //Logger.warn("R * A:\n" + ra.print)
//    ra.filter()
//    //Logger.warn("R * A:\n" + ra.print)
//    stencils += ra
//
//    val rap = ra.mul(prolong)
//    //Logger.warn("R * A * P:\n" + rap.print)
//    rap.squash()
//    //Logger.warn("R * A * P:\n" + rap.print)
//    rap.filter()
//    //Logger.warn("R * A * P:\n" + rap.print)
//    stencils += rap
//
//    def coarsen(op : MatFromStencil) = {
//      var rec = op
//      rec = restrict.mul(rec)
//      rec.squash()
//      rec.filter()
//      rec = rec.mul(prolong)
//      rec.squash()
//      rec.filter()
//      rec
//    }
//
//    val rrapp = coarsen(rap)
//    stencils += rrapp
//    val rrrrapppp = coarsen(coarsen(rrapp))
//    stencils += rrrrapppp
//    val r8ap8 = coarsen(coarsen(coarsen(coarsen(rrrrapppp))))
//    stencils += r8ap8
//
//    val rp = restrict.mul(prolong)
//    rp.squash()
//    rp.filter()
//    stencils += rp
//
//    stencils.foreach(_.entries.foreach(entry => ReplaceRealWithInt.applyStandalone(entry.index)))
//
//    Logger.warn("R * A:\n" + ra.print)
//    Logger.warn("R * A * P:\n" + rap.print)
//
//    Logger.warn("RR * A * PP:\n" + rrapp.print)
//    Logger.warn("RRRR * A * PPPP:\n" + rrrrapppp.print)
//    Logger.warn("R^8 * A * P^8:\n" + r8ap8.print)
//
//    Logger.warn("R * P:\n" + rp.print)
//    Logger.warn("sum per line (RP): " + IR_SimplifyExpression.simplifyFloatingExpr(IR_Addition(rp.entries.map(_.coeff))))
//
////    Logger.warn("R * A * P:\n" + rap.toIRStencil().printStencilToStr())
//  }
//
//  case class MatAccess(var stencil : MatFromStencil) extends IR_Expression {
//    override def datatype : IR_Datatype = ???
//    override def prettyprint(out : PpStream) : Unit = ???
//  }
//
//  object ResolveStencilOperations extends DefaultStrategy("Resolve operations involving stencils") {
//    this += new Transformation("Resolve", {
//      case add : IR_Addition if add.summands.exists(_.isInstanceOf[MatAccess]) =>
//        val (stencilAccesses, other) : (ListBuffer[IR_Expression], ListBuffer[IR_Expression]) = add.summands.partition(_.isInstanceOf[MatAccess])
//        val stencils = stencilAccesses.map(_.asInstanceOf[MatAccess].stencil)
//        var newStencil = stencils.remove(stencils.length - 1)
//        while (stencils.nonEmpty)
//          newStencil = newStencil.add(stencils.remove(stencils.length - 1))
//
//        add.summands = MatAccess(newStencil) +: other
//        add
//
//      case mul : IR_Multiplication if mul.factors.exists(_.isInstanceOf[MatAccess]) =>
//
//        mul
//    })
//  }
//
////  def checkTypes() = {
////    type L2Expression = GenExpression[L2]
////    type L2Add = GenAdd[L2]
////    val L2Add = GenAdd[L2]
////    type GenAdd[IR] = MyAdd[IR]
////
////    val l2Add = L2Add(NoEx[L2], NoEx[L2])
////    l2Add.print
////  }
//
//  def main(args : Array[String]) : Unit = {
//    //check1D()
//    //check2D()
//
////    checkTypes()
//
//    Logger.warn("Done")
//  }
//}

//trait Layer//[next <: Layer[_]]
//
//trait L2 extends Layer//[L3]
//
//trait L3 extends Layer//[L4]
//
//trait L4 extends Layer//[IR]
//
//trait IR extends Layer//[_]
//
//trait GenExpression[@specialized layer <: Layer] {
//  //def progress() : next
//}
//
//case class GenAdd[@specialized layer <: Layer](var left : GenExpression[layer], var right : GenExpression[layer]) extends GenExpression[layer] {
//  //override def progress : layer
//  def print = Logger.warn(left.getClass.getName)
//}
//
//case class MyAdd[@specialized IR]() {
//
//}
//
//case class NoEx[@specialized layer <: Layer]() extends GenExpression[layer] {}
//
//package object testbed {
//}

//object IR_ReplaceVariableAccess extends QuietDefaultStrategy("Replace something with something else") {
//  var toReplace : String = ""
//  var replacement : Node = IR_VariableAccess("", IR_UnknownDatatype) // to be overwritten
//
//  this += new Transformation("Search and replace", {
//// TODO: rely only on IR_VariableAccess => eliminate IR_StringLiteral occurrences
//    case IR_StringLiteral(s) if s == toReplace                  => Duplicate(replacement)
//    case access : IR_VariableAccess if access.name == toReplace => Duplicate(replacement)
//  }, false)
//}

//import exastencils.baseExt.ir._
//import exastencils.config._
//import exastencils.core._
//import exastencils.datastructures.Transformation._
//import exastencils.datastructures._
//import exastencils.operator.ir.IR_Stencil
//import exastencils.operator.ir.IR_StencilEntry
//import exastencils.optimization.ir.IR_GeneralSimplify
//import exastencils.prettyprinting._
//import exastencils.stencil.ir._
//
//object Testbed {
//  def test : Unit = {
//    //////////////////////////////////////////////////////////////////////////////
//    val test = new java.util.IdentityHashMap[Node, Any]()
//    new exastencils.datastructures.DefaultStrategy("TestStrategy") {
//      this += new exastencils.datastructures.Transformation("test", {
//        case n =>
//          if (test.containsKey(n))
//            println("error: " + n.getClass() + "   " + n)
//          test.put(n, null)
//          n
//      })
//    }.apply()
//    println("ende...")
//    return
//    //////////////////////////////////////////////////////////////////////////////
//  }
//
//  object ResolveCoordinates0 extends DefaultStrategy("ResolveCoordinates0") {
//    var replacement : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // to be overwritten
//
//    Knowledge.dimensionality match {
//      case 1 => this += new Transformation("SearchAndReplace", {
//        case IR_StringLiteral("x0") => replacement(0)
//      })
//      case 2 => this += new Transformation("SearchAndReplace", {
//        case IR_StringLiteral("x0") => replacement(0)
//        case IR_StringLiteral("y0") => replacement(1)
//      })
//      case 3 => this += new Transformation("SearchAndReplace", {
//        case IR_StringLiteral("x0") => replacement(0)
//        case IR_StringLiteral("y0") => replacement(1)
//        case IR_StringLiteral("z0") => replacement(2)
//      })
//    }
//  }
//
//  def rap(R : IR_Stencil, A : IR_Stencil, P : IR_Stencil) : IR_Stencil = {
//
//    var RAP = IR_StencilStencilConvolution(A, R).expand.inner.stencil
//    //    for (e <- RAP.entries)
//    //      println(e.offset.prettyprint + "\t>>\t" + e.weight.prettyprint)
//
//    RAP = IR_StencilStencilConvolution(P, RAP).expand.inner.stencil
//
//    //var RAP : Stencil = StencilStencilConvolution(P, StencilStencilConvolution(A, R).expand).expand
//
//    for (e <- RAP.entries)
//      println(e.offset.prettyprint() + "\t>>\t" + e.coefficient.prettyprint)
//
//    /*{
//      var entries : ListBuffer[StencilEntry] = ListBuffer()
//
//      for (re <- AP.entries) {
//        for (le <- P.entries) {
//          var rightOffset = Duplicate(re.offset)
//          var leftOffset = Duplicate(le.offset)
//          for (d <- 0 until Knowledge.dimensionality)
//            leftOffset(d) = dimToString(d) / 2 + leftOffset(d)
//
//          var combOff = leftOffset
//          ResolveCoordinates.replacement = rightOffset
//          ResolveCoordinates.apply(Some(combOff))
//
//          var combCoeff : Expression = (re.weight * le.weight)
//          SimplifyStrategy.doUntilDoneStandalone(combOff)
//          SimplifyStrategy.doUntilDoneStandalone(combCoeff)
//          var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
//          if (addToEntry.isDefined) {
//            combCoeff += addToEntry.get.weight
//            SimplifyStrategy.doUntilDoneStandalone(combCoeff)
//            addToEntry.get.weight = combCoeff
//          } else entries += new StencilEntry(combOff, combCoeff)
//        }
//      }
//
//      new Stencil(P.identifier + "_" + AP.identifier, P.level, entries)
//    }*/
//
//    //    for (e <- RAP.entries) {
//    //      ResolveCoordinates0.replacement = new MultiIndex(0, 0, 0, 0)
//    //      ResolveCoordinates0.apply(Some(e.offset))
//    //    }
//
//    /*var AP : Stencil = StencilStencilConvolution(A, P).expand
//    println(AP.printStencilToStr)
//
//    var RAP : Stencil = {
//
//      var entries : ListBuffer[StencilEntry] = ListBuffer()
//
//      for (re <- AP.entries) {
//        for (le <- P.entries) {
//          var combOff : MultiIndex = re.offset + le.offset
//          var combCoeff : Expression = (re.weight * le.weight)
//
//          var valid = true
//          for (d <- 0 until Knowledge.dimensionality) {
//            if (0 != SimplifyExpression.evalIntegral(combOff(d)) % 2) valid = false
//            combOff(d) /= 2
//          }
//
//          if (valid) {
//            SimplifyStrategy.doUntilDoneStandalone(combOff)
//            SimplifyStrategy.doUntilDoneStandalone(combCoeff)
//            var addToEntry = entries.find(e => e.offset match { case o if (combOff == o) => true; case _ => false })
//            if (addToEntry.isDefined) {
//              combCoeff += addToEntry.get.weight
//              SimplifyStrategy.doUntilDoneStandalone(combCoeff)
//              addToEntry.get.weight = SimplifyExpression.evalFloating(combCoeff)
//              //              combCoeff += addToEntry.get.weight
//              //              SimplifyStrategy.doUntilDoneStandalone(combCoeff)
//              //              addToEntry.get.weight = combCoeff
//            } else entries += new StencilEntry(combOff, combCoeff)
//          }
//        }
//      }
//
//      new Stencil(P.identifier + "_" + AP.identifier, P.level, entries)
//    }*/
//    RAP
//  }
//
//  def main(args : Array[String]) : Unit = {
//    Knowledge.dimensionality = 2
//
//    var A : IR_Stencil = new IR_Stencil("A", 5,
//      if (false) {
//        ListBuffer(
//          new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 3.0),
//          new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), -0.5),
//          new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), -0.5),
//          new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), -0.5),
//          new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), -0.5),
//          new IR_StencilEntry(IR_ExpressionIndex(-1, -1, 0), -0.25),
//          new IR_StencilEntry(IR_ExpressionIndex(-1, 1, 0), -0.25),
//          new IR_StencilEntry(IR_ExpressionIndex(1, -1, 0), -0.25),
//          new IR_StencilEntry(IR_ExpressionIndex(1, 1, 0), -0.25))
//      } else if (true) {
//        ListBuffer(
//          new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 4.0),
//          new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), -1.0),
//          new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), -1.0),
//          new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), -1.0),
//          new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), -1.0))
//      } else {
//        ListBuffer(
//          new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), "C"),
//          new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), "W"),
//          new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), "E"),
//          new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), "S"),
//          new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), "N"))
//      })
//
//    var R : IR_Stencil = new IR_Stencil("R", 4, ListBuffer(
//      new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 1.0),
//      new IR_StencilEntry(IR_ExpressionIndex(-1, 0, 0), 0.5),
//      new IR_StencilEntry(IR_ExpressionIndex(1, 0, 0), 0.5),
//      new IR_StencilEntry(IR_ExpressionIndex(0, -1, 0), 0.5),
//      new IR_StencilEntry(IR_ExpressionIndex(0, 1, 0), 0.5),
//      new IR_StencilEntry(IR_ExpressionIndex(-1, -1, 0), 0.25),
//      new IR_StencilEntry(IR_ExpressionIndex(-1, 1, 0), 0.25),
//      new IR_StencilEntry(IR_ExpressionIndex(1, -1, 0), 0.25),
//      new IR_StencilEntry(IR_ExpressionIndex(1, 1, 0), 0.25)))
//
//    var P : IR_Stencil = new IR_Stencil("P", 4, ListBuffer(
//      new IR_StencilEntry(IR_ExpressionIndex(0, 0, 0), 0.25),
//      new IR_StencilEntry(IR_ExpressionIndex("x" Mod 2, 0, 0), 0.25),
//      new IR_StencilEntry(IR_ExpressionIndex(0, "y" Mod 2, 0), 0.25),
//      new IR_StencilEntry(IR_ExpressionIndex("x" Mod 2, "y" Mod 2, 0), 0.25)))
//
//    println(R.printStencilToStr)
//    println(A.printStencilToStr)
//    //P.printStencil
//
//    var RAP = A
//
//    for (i <- 0 until 1) {
//      RAP.level = R.level + 1
//      RAP = rap(R, RAP, P)
//      println(RAP.printStencilToStr)
//    }
//
//    return
//
//    /*  val index = new MultiIndex(1, 2, 3)
//  val aabb = new IndexRange(new MultiIndex(0, 0, 0), new MultiIndex(33, 33, 33))
//
//  val node : Statement = Knowledge.dimensionality match {
//    case 1 => (index(0))
//    case 2 => (index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
//    case 3 => (index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
//  }
//
//  println(node)
//  println(node.prettyprint)
//
//  do { SimplifyStrategy.apply(Some(node), StateManager.History.currentToken) }
//  while (SimplifyStrategy.results.last._2.replacements > 0) // FIXME: cleaner code
//
//  println(node)
//  println(node.prettyprint)
//*/
//
//    var statements = new ListBuffer[IR_Statement]
//
//    statements +=
//      """void tet_gs_coeff_1c(double* u, double* f, double *koe, double* stiff, int *p_tsize) {
//
//   int tsize = p_tsize[0];
//   int mp, tp, bp;
//   int mp_mr, mp_tr, mp_br;
//   int tp_mr, tp_br;
//   int bp_mr, bp_tr;
//   bp = 0;
//   mp = PLAINSIZE(tsize);
//   tp = mp + PLAINSIZE(tsize - 1);
//   //HS const int stiffsize = 1;
//   //HS  double** c = new double*[stiffsize]; //stiff.size
//   double* c = new double[6 * 4 * 4];
//   double* stencil = new double[16];
//   double k_tw_tc, k_tc_tse, k_tc_mn, k_ts_mc, k_tse_mse, k_tw_mw, k_mnw_mc, k_mc_me, k_ms_mse, k_mw_ms;
//   double k_mc_bc, k_mn_bn, k_mse_be, k_mw_bnw, k_bnw_bn, k_bn_be;
//   double k_el;
//   //HS
//   for (int i = 0; i < 6 * 4 * 4; ++i) {
//      c[i] = 0.25 * stiff[i];
//   }
//   for (int k = 1; k < (tsize - 3); ++k) {
//      bp_mr = bp + tsize - k + 1;
//      bp_tr = bp + 2 * (tsize - k + 1) - 1;
//      mp_br = mp;
//      mp_mr = mp + tsize - k;
//      mp_tr = mp + 2 * (tsize - k) - 1;
//      tp_br = tp;
//      tp_mr = tp + tsize - k - 1;
//      for (int j = 1; j < (tsize - k - 2); ++j) {
//#pragma simd
//         for (int i = 1; i < (tsize - j - k - 1); i = i + 1) {
//            // 16 FLOPs
//            k_tw_tc   = koe[tp_mr + i - 1] + koe[tp_mr + i];
//            k_tc_tse  = koe[tp_mr + i]     + koe[tp_br + i + 1];
//            k_tc_mn   = koe[tp_mr + i]     + koe[mp_tr + i];
//            k_ts_mc   = koe[tp_br + i]     + koe[mp_mr + i];
//            k_tse_mse = koe[tp_br + i + 1] + koe[mp_br + i + 1];
//            k_tw_mw   = koe[tp_mr + i - 1] + koe[mp_mr + i - 1];
//            k_mnw_mc  = koe[mp_tr + i - 1] + koe[mp_mr + i];
//            k_mc_me   = koe[mp_mr + i]     + koe[mp_mr + i + 1];
//            k_ms_mse  = koe[mp_br + i]     + koe[mp_br + i + 1];
//            k_mw_ms   = koe[mp_mr + i - 1] + koe[mp_br + i];
//            k_mc_bc   = koe[mp_mr + i]     + koe[bp_mr + i];
//            k_mn_bn   = koe[mp_tr + i]     + koe[bp_tr + i];
//            k_mse_be  = koe[mp_br + i + 1] + koe[bp_mr + i + 1];
//            k_mw_bnw  = koe[mp_mr + i - 1] + koe[bp_tr + i - 1];
//            k_bnw_bn  = koe[bp_tr + i - 1] + koe[bp_tr + i];
//            k_bn_be   = koe[bp_tr + i]     + koe[bp_mr + i + 1];
//            // 196-15 = 181 Flops
//      """
//
//    var aabb = IR_ExpressionIndexRange(IR_ExpressionIndex(0, 0, 0), IR_ExpressionIndex(4, 4, 4))
//    var k : IR_Expression = "k_el"
//    for (group <- 0 to 5) {
//      for (position <- 0 to 3) {
//        statements += IR_Assignment(k, "k_tc_mn + k_mc_me")
//        for (i <- 0 to 4) {
//          statements += IR_Assignment(IR_ArrayAccess("stencil", "tet_mc"), k * IR_ArrayAccess("c", aabb.linearizeIndex(IR_ExpressionIndex(i, position, group))), "+=")
//        }
//      }
//    }
//
//    /*
//            /////////////////////////////////////////
//            // group zero element
//            /////////////////////////////////////////
//            k_el = k_tc_mn + k_mc_me;
//            // position zero
//            stencil[tet_mc] = k_el * c[0 * 4 * 4 + 0 * 4 + 0];
//            stencil[tet_me] = k_el * c[0 * 4 * 4 + 0 * 4 + 1];
//            stencil[tet_mn] = k_el * c[0 * 4 * 4 + 0 * 4 + 2];
//            stencil[tet_tc] = k_el * c[0 * 4 * 4 + 0 * 4 + 3];
//            k_el = k_mnw_mc + k_tw_mw;
//            // position one
//            stencil[tet_mw] = k_el * c[0 * 4 * 4 + 1 * 4 + 0];
//            stencil[tet_mc] += k_el * c[0 * 4 * 4 + 1 * 4 + 1];
//            stencil[tet_mnw] = k_el * c[0 * 4 * 4 + 1 * 4 + 2];
//            stencil[tet_tw] = k_el * c[0 * 4 * 4 + 1 * 4 + 3];
//            k_el = k_ts_mc + k_ms_mse;
//            // position two
//            stencil[tet_ms] = k_el * c[0 * 4 * 4 + 2 * 4 + 0];
//            stencil[tet_mse] = k_el * c[0 * 4 * 4 + 2 * 4 + 1];
//            stencil[tet_mc] += k_el * c[0 * 4 * 4 + 2 * 4 + 2];
//            stencil[tet_ts] = k_el * c[0 * 4 * 4 + 2 * 4 + 3];
//            k_el = k_mc_bc + k_bn_be;
//            // position three
//            stencil[tet_bc] = k_el * c[0 * 4 * 4 + 3 * 4 + 0];
//            stencil[tet_be] = k_el * c[0 * 4 * 4 + 3 * 4 + 1];
//            stencil[tet_bn] = k_el * c[0 * 4 * 4 + 3 * 4 + 2];
//            stencil[tet_mc] += k_el * c[0 * 4 * 4 + 3 * 4 + 3];
//            /////////////////////////////////////////
//            // group one element
//            /////////////////////////////////////////
//            k_el = k_tw_tc + k_ts_mc;
//            // position zero
//            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 0 * 4 + 0];
//            stencil[tet_ts] += k_el * c[1 * 4 * 4 + 0 * 4 + 1];
//            stencil[tet_tw] += k_el * c[1 * 4 * 4 + 0 * 4 + 2];
//            stencil[tet_tc] += k_el * c[1 * 4 * 4 + 0 * 4 + 3];
//            k_el = k_mnw_mc + k_mn_bn;
//            // position one
//            stencil[tet_bn] += k_el * c[1 * 4 * 4 + 1 * 4 + 0];
//            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 1 * 4 + 1];
//            stencil[tet_mnw] += k_el * c[1 * 4 * 4 + 1 * 4 + 2];
//            stencil[tet_mn] += k_el * c[1 * 4 * 4 + 1 * 4 + 3];
//            k_el = k_mc_me + k_mse_be;
//            // position two
//            stencil[tet_be] += k_el * c[1 * 4 * 4 + 2 * 4 + 0];
//            stencil[tet_mse] += k_el * c[1 * 4 * 4 + 2 * 4 + 1];
//            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 2 * 4 + 2];
//            stencil[tet_me] += k_el * c[1 * 4 * 4 + 2 * 4 + 3];
//            k_el = k_mw_ms + k_mc_bc;
//            // position three
//            stencil[tet_bc] += k_el * c[1 * 4 * 4 + 3 * 4 + 0];
//            stencil[tet_ms] += k_el * c[1 * 4 * 4 + 3 * 4 + 1];
//            stencil[tet_mw] += k_el * c[1 * 4 * 4 + 3 * 4 + 2];
//            stencil[tet_mc] += k_el * c[1 * 4 * 4 + 3 * 4 + 3];
//            /////////////////////////////////////////
//            // group two element
//            /////////////////////////////////////////
//            k_el = k_tw_tc + k_mnw_mc;
//            // position zero
//            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 0 * 4 + 0];
//            stencil[tet_mnw] += k_el * c[2 * 4 * 4 + 0 * 4 + 1];
//            stencil[tet_tw] += k_el * c[2 * 4 * 4 + 0 * 4 + 2];
//            stencil[tet_tc] += k_el * c[2 * 4 * 4 + 0 * 4 + 3];
//            k_el = k_ts_mc + k_tse_mse;
//            // position one
//            stencil[tet_mse] += k_el * c[2 * 4 * 4 + 1 * 4 + 0];
//            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 1 * 4 + 1];
//            stencil[tet_ts] += k_el * c[2 * 4 * 4 + 1 * 4 + 2];
//            stencil[tet_tse] = k_el * c[2 * 4 * 4 + 1 * 4 + 3];
//            k_el = k_mc_me + k_bn_be;
//            // position two
//            stencil[tet_be] += k_el * c[2 * 4 * 4 + 2 * 4 + 0];
//            stencil[tet_bn] += k_el * c[2 * 4 * 4 + 2 * 4 + 1];
//            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 2 * 4 + 2];
//            stencil[tet_me] += k_el * c[2 * 4 * 4 + 2 * 4 + 3];
//            k_el = k_mc_bc + k_mw_bnw;
//            // position three
//            stencil[tet_bc] += k_el * c[2 * 4 * 4 + 3 * 4 + 0];
//            stencil[tet_bnw] = k_el * c[2 * 4 * 4 + 3 * 4 + 1];
//            stencil[tet_mw] += k_el * c[2 * 4 * 4 + 3 * 4 + 2];
//            stencil[tet_mc] += k_el * c[2 * 4 * 4 + 3 * 4 + 3];
//            /////////////////////////////////////////
//            // group three element
//            /////////////////////////////////////////
//            k_el = k_tc_tse + k_mc_me;
//            // position zero
//            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 0 * 4 + 0];
//            stencil[tet_me] += k_el * c[3 * 4 * 4 + 0 * 4 + 1];
//            stencil[tet_tse] += k_el * c[3 * 4 * 4 + 0 * 4 + 2];
//            stencil[tet_tc] += k_el * c[3 * 4 * 4 + 0 * 4 + 3];
//            k_el = k_tw_mw + k_ts_mc;
//            // position one
//            stencil[tet_mw] += k_el * c[3 * 4 * 4 + 1 * 4 + 0];
//            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 1 * 4 + 1];
//            stencil[tet_ts] += k_el * c[3 * 4 * 4 + 1 * 4 + 2];
//            stencil[tet_tw] += k_el * c[3 * 4 * 4 + 1 * 4 + 3];
//            k_el = k_mnw_mc + k_bnw_bn;
//            // position two
//            stencil[tet_bnw] += k_el * c[3 * 4 * 4 + 2 * 4 + 0];
//            stencil[tet_bn] += k_el * c[3 * 4 * 4 + 2 * 4 + 1];
//            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 2 * 4 + 2];
//            stencil[tet_mnw] += k_el * c[3 * 4 * 4 + 2 * 4 + 3];
//            k_el = k_mse_be + k_mc_bc;
//            // position three
//            stencil[tet_bc] += k_el * c[3 * 4 * 4 + 3 * 4 + 0];
//            stencil[tet_be] += k_el * c[3 * 4 * 4 + 3 * 4 + 1];
//            stencil[tet_mse] += k_el * c[3 * 4 * 4 + 3 * 4 + 2];
//            stencil[tet_mc] += k_el * c[3 * 4 * 4 + 3 * 4 + 3];
//            /////////////////////////////////////////
//            // group four element
//            /////////////////////////////////////////
//            k_el = k_tc_tse + k_ts_mc;
//            // position zero
//            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 0 * 4 + 0];
//            stencil[tet_ts] += k_el * c[4 * 4 * 4 + 0 * 4 + 1];
//            stencil[tet_tse] += k_el * c[4 * 4 * 4 + 0 * 4 + 2];
//            stencil[tet_tc] += k_el * c[4 * 4 * 4 + 0 * 4 + 3];
//            k_el = k_mc_me + k_mn_bn;
//            // position one
//            stencil[tet_bn] += k_el * c[4 * 4 * 4 + 1 * 4 + 0];
//            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 1 * 4 + 1];
//            stencil[tet_me] += k_el * c[4 * 4 * 4 + 1 * 4 + 2];
//            stencil[tet_mn] += k_el * c[4 * 4 * 4 + 1 * 4 + 3];
//            k_el = k_mnw_mc + k_mw_bnw;
//            // position two
//            stencil[tet_bnw] += k_el * c[4 * 4 * 4 + 2 * 4 + 0];
//            stencil[tet_mw] += k_el * c[4 * 4 * 4 + 2 * 4 + 1];
//            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 2 * 4 + 2];
//            stencil[tet_mnw] += k_el * c[4 * 4 * 4 + 2 * 4 + 3];
//            k_el = k_ms_mse + k_mc_bc;
//            // position three
//            stencil[tet_bc] += k_el * c[4 * 4 * 4 + 3 * 4 + 0];
//            stencil[tet_ms] += k_el * c[4 * 4 * 4 + 3 * 4 + 1];
//            stencil[tet_mse] += k_el * c[4 * 4 * 4 + 3 * 4 + 2];
//            stencil[tet_mc] += k_el * c[4 * 4 * 4 + 3 * 4 + 3];
//            /////////////////////////////////////////
//            // group five element
//            /////////////////////////////////////////
//            k_el = k_tc_mn + k_mnw_mc;
//            // position zero
//            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 0 * 4 + 0];
//            stencil[tet_mnw] += k_el * c[5 * 4 * 4 + 0 * 4 + 1];
//            stencil[tet_mn] += k_el * c[5 * 4 * 4 + 0 * 4 + 2];
//            stencil[tet_tc] += k_el * c[5 * 4 * 4 + 0 * 4 + 3];
//            k_el = k_tse_mse + k_mc_me;
//            // position one
//            stencil[tet_mse] += k_el * c[5 * 4 * 4 + 1 * 4 + 0];
//            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 1 * 4 + 1];
//            stencil[tet_me] += k_el * c[5 * 4 * 4 + 1 * 4 + 2];
//            stencil[tet_tse] += k_el * c[5 * 4 * 4 + 1 * 4 + 3];
//            k_el = k_ts_mc + k_mw_ms;
//            // position two
//            stencil[tet_ms] += k_el * c[5 * 4 * 4 + 2 * 4 + 0];
//            stencil[tet_mw] += k_el * c[5 * 4 * 4 + 2 * 4 + 1];
//            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 2 * 4 + 2];
//            stencil[tet_ts] += k_el * c[5 * 4 * 4 + 2 * 4 + 3];
//            k_el = k_mc_bc + k_bnw_bn;
//            // position three
//            stencil[tet_bc] += k_el * c[5 * 4 * 4 + 3 * 4 + 0];
//            stencil[tet_bnw] += k_el * c[5 * 4 * 4 + 3 * 4 + 1];
//            stencil[tet_bn] += k_el * c[5 * 4 * 4 + 3 * 4 + 2];
//            stencil[tet_mc] += k_el * c[5 * 4 * 4 + 3 * 4 + 3];
//*/
//    statements +=
//      """// compute center weight quotient
//            stencil[tet_mcq] = 1.0 / stencil[tet_mc];
//            u[mp_mr + i] =   stencil[0]  *(f[mp_mr + i] -
//                  stencil[1]  * u[mp_mr + i + 1] -
//                  stencil[2]  * u[mp_tr + i - 1] -
//                  stencil[3]  * u[mp_tr + i] -
//                  stencil[4]  * u[tp_br + i] -
//                  stencil[5]  * u[tp_br + i + 1] -
//                  stencil[6]  * u[tp_mr + i - 1] -
//                  stencil[7]  * u[tp_mr + i] -
//                  stencil[8]  * u[bp_mr + i] -
//                  stencil[9]  * u[bp_mr + i + 1] -
//                  stencil[10] * u[bp_tr + i - 1] -
//                  stencil[11] * u[bp_tr + i] -
//                  stencil[12] * u[mp_br + i] -
//                  stencil[13] * u[mp_br + i + 1] -
//                  stencil[14] * u[mp_mr + i - 1]);
//         }  // i
//         bp_mr = bp_tr;
//         bp_tr = bp_tr + tsize - j - k;
//         mp_br = mp_mr;
//         mp_mr = mp_tr;
//         mp_tr = mp_tr + tsize - j - k - 1;
//         tp_br = tp_mr;
//         tp_mr = tp_mr + tsize - j - k - 1;
//      }  // j
//      bp = mp;
//      mp = tp;
//      tp = tp + PLAINSIZE(tsize-k-1);
//   }  // k
//   delete[] stencil;
//   stencil=0;
//   delete[] c;
//   c=0;
//}  // tet_gs_coeff
//      """
//
//    var root = IR_Scope(statements)
//
//    StateManager.root_ = root
//
//    IR_GeneralSimplify.doUntilDone()
//
//    Settings.outputPath = "Heap/"
//    var printer = PrettyprintingManager.getPrinter("tet_gs_coeff_gen.cc")
//    printer << root.prettyprint
//    PrettyprintingManager.finish
//  }
//}
