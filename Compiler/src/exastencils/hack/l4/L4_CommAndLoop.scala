package exastencils.hack.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.communication.l4.L4_Communicate
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldFieldConvolution

//
//// TODO: split into separate strategies; move to more appropriate files; integrate
//
///// L4_AddCommunicationAndLoopStatements
//
//object L4_AddCommunicationAndLoopStatements extends DefaultStrategy("Add communication and loop statements around field assignments") {
//
//  this += new Transformation("Add loops around field assignments", {
//    case assignment @ L4_Assignment(lhs : L4_FieldAccess, rhs, op, cond) => {
//      var ret : L4_Statement = L4_LoopOverField(Duplicate(lhs), None, false, cond, None, None, None, List(assignment), None, List(), List())
//
//      if (false)
//        ret = L4_LoopOverFragments(List(ret), None)
//
//      ret
//    }
//  }, false /* recursion must be switched of due to wrapping mechanism */)
//
//  // implements a pull strategy
//  this += new Transformation("Add communication to loops", {
//    case loop : L4_LoopOverField => {
//      CollectCommInformation.applyStandalone(loop)
//      val collector = CollectCommInformation.collector
//
//      // find all fields read outside the iteration space
//      var fieldsToConsider = ListBuffer[L4_Field]()
//      for (fieldData <- collector.readExtentMax)
//        if (fieldData._2.count(_ != 0) > 0)
//          fieldsToConsider += fieldData._1
//
//      var commStatements = ListBuffer[L4_Communicate]()
//
//      for (field <- fieldsToConsider) {
//        var targets = ListBuffer[L4_CommunicateTarget]()
//        targets += L4_CommunicateTarget("ghost", None, Some(L4_Index(collector.readExtentMax(field))))
//        commStatements += L4_Communicate(
//          L4_FieldAccess(field.name, L4_SingleLevel(field.level), L4_ActiveSlot()),
//          "both",
//          targets.toList,
//          None)
//        // TODO: append potential assignment condition to communicate statement
//      }
//
//      var finalStmts = ListBuffer[L4_Statement]()
//
//      if (false) { // append as preComms
//        loop.preComms ++= commStatements
//        finalStmts += loop
//      } else { // prepend comm statements
//        finalStmts ++= commStatements.map(s => s : L4_Statement)
//        finalStmts += loop
//      }
//
//      for (field <- collector.writeExtentMax.keys)
//        if (field.boundary.isDefined)
//          finalStmts += L4_ApplyBC(L4_FieldAccess(field))
//
//      finalStmts
//    }
//
//    // FIXME: handle reductions
//    // FIXME: handle stencil fields
//    // FIXME: handle region loops
//  }, false)
//}
//
////object AdaptFieldLayouts extends DefaultStrategy("AdaptFieldLayouts") {
//object L4_CollectCommInformation extends QuietDefaultStrategy("Collect information relevant for adding communication statements") {
//  var collector = new L4_FieldAccessRangeCollector()
//
//  override def apply(node : Option[Node] = None) = {
//    collector.reset()
//    this.register(collector)
//    super.apply(node)
//    this.unregister(collector)
//    collector.adaptNodeBasedFields
//  }
//
//  override def applyStandalone(node : Node) = {
//    collector.reset()
//    this.register(collector)
//    super.applyStandalone(node)
//    this.unregister(collector)
//    collector.adaptNodeBasedFields
//  }
//
//  this += new Transformation("Collect", PartialFunction.empty)
//}
//
//object L4_AdaptFieldLayouts extends DefaultStrategy("AdaptFieldLayouts") {
//  var collector = new L4_FieldAccessRangeCollector()
//
//  override def apply(node : Option[Node] = None) = {
//    collector.reset()
//    this.register(collector)
//    super.apply(node)
//    this.unregister(collector)
//    collector.adaptNodeBasedFields
//    actuallyAdapt
//  }
//
//  override def applyStandalone(node : Node) = {
//    collector.reset()
//    this.register(collector)
//    super.applyStandalone(node)
//    this.unregister(collector)
//    collector.adaptNodeBasedFields
//    actuallyAdapt
//  }
//
//  def actuallyAdapt = {
//    var unreferencedFields = ListBuffer[L4_Field]()
//
//    for (field <- L4_FieldCollection.objects) {
//      val defLayout = field.fieldLayout
//
//      val fieldIsRead = collector.readExtentMin.contains(field)
//      val fieldIsWritten = collector.writeExtentMin.contains(field)
//      if (!fieldIsRead)
//        Logger.warn(s"Found l4 field without read access: ${ field.name } on level ${ field.level }")
//      if (!fieldIsWritten)
//        Logger.warn(s"Found l4 field without write access: ${ field.name } on level ${ field.level }")
//
//      if (!fieldIsRead && !fieldIsWritten) {
//        unreferencedFields += field
//      } else {
//        val numGhostLayersLeft = (fieldIsRead, fieldIsWritten) match {
//          case (true, false) => collector.readExtentMin(field).map(-1 * _)
//          case (false, true) => collector.writeExtentMin(field).map(-1 * _)
//          case (true, true)  => (collector.writeExtentMin(field), collector.readExtentMin(field)).zipped.map(math.min).map(-1 * _)
//        }
//        val numGhostLayersRight = (fieldIsRead, fieldIsWritten) match {
//          case (true, false) => collector.readExtentMax(field)
//          case (false, true) => collector.writeExtentMax(field)
//          case (true, true)  => (collector.writeExtentMax(field), collector.readExtentMax(field)).zipped.map(math.max)
//        }
//
//        // adapt for bc's of field if required
//        if (L4_NoBC != field.boundary) { // TODO: update for new bc classes; Neumann for node; warn for fcts
//          for (i <- numGhostLayersLeft.indices) {
//            val localization = defLayout.discretization.toLowerCase
//            if ("node" == localization || s"face_${ IR_DimToString(i) }" == localization) {
//              // node type localization doesn't require ghost layers for boundary handling - apart from Neumann
//              field.boundary match {
//                case L4_NeumannBC(order) => {
//                  numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
//                  numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)
//                }
//                case _                   =>
//              }
//            } else if ("cell" == localization || "face_x" == localization || "face_y" == localization || "face_z" == localization) {
//              // cell type localization always requires (at least) on ghost layer for implementing boundary conditions
//              numGhostLayersLeft(i) = math.max(numGhostLayersLeft(i), 1)
//              numGhostLayersRight(i) = math.max(numGhostLayersRight(i), 1)
//            } else {
//              Logger.warn(s"Encountered unknown localization: $localization")
//            }
//
//          }
//        }
//
//        val newLayoutName = s"${ defLayout.name }__${ numGhostLayersLeft.mkString("_") }__${ numGhostLayersRight.mkString("_") }"
//
//        // add layout if not existent yet
//        if (!L4_FieldLayoutCollection.exists(newLayoutName, field.level)) {
//          val newLayout = Duplicate(defLayout)
//          newLayout.name = newLayoutName
//          val numGhostLayers = (numGhostLayersLeft, numGhostLayersRight).zipped.map(math.max)
//          newLayout.ghostLayers = L4_Index(numGhostLayers)
//          newLayout.communicatesGhosts = numGhostLayers.count(_ != 0) > 0
//          // FIXME: how to determine if duplicates should communicate? activate by default?
//          L4_FieldLayoutCollection.add(newLayout)
//        }
//
//        // assign layout to field
//        field.fieldLayout = newLayoutName
//      }
//    }
//
//    // cleanup - remove unused fields and field layouts // TODO: control via knowledge parameter
//    if (false) {
//      L4_FieldCollection.objects = L4_FieldCollection.objects.filter(field => !unreferencedFields.contains(field))
//      L4_FieldLayoutCollection.objects = L4_FieldLayoutCollection.objects.filter(layout => L4_FieldCollection.objects.exists(field => field.fieldLayout == layout.name && field.level == layout.level))
//    }
//  }
//
//  this += new Transformation("Collect", PartialFunction.empty)
//}
//
//class L4_FieldAccessRangeCollector() extends Collector {
//  // TODO: respect slots if used for comm stuff
//
//  var beginOffset = Array[Int]()
//  var endOffset = Array[Int]()
//  var contractionOffsetBegin = Array[Int]()
//  var contractionOffsetEnd = Array[Int]()
//
//  var readExtentMin = HashMap[L4_Field, Array[Int]]()
//  var readExtentMax = HashMap[L4_Field, Array[Int]]()
//  var writeExtentMin = HashMap[L4_Field, Array[Int]]()
//  var writeExtentMax = HashMap[L4_Field, Array[Int]]()
//
//  var ignore : Boolean = true
//
//  def adaptNodeBasedFields() = {
//    def adapt(map : HashMap[L4_Field, Array[Int]]) = {
//      for (field <- map) {
//        // deduct offset due to node-based discretizations
//        field._1.fieldLayout.discretization.toLowerCase match {
//          case "node"   => field._2.transform(_ - 1)
//          case "face_x" => field._2(0) -= 1
//          case "face_y" => field._2(1) -= 1
//          case "face_z" => field._2(2) -= 1
//          case "cell"   =>
//          case other    => Logger.warn(s"Found unknown discretization $other")
//        }
//      }
//    }
//
//    adapt(readExtentMax)
//    adapt(writeExtentMax)
//  }
//
//  def minValuesForExprIndex(index : L4_ExpressionIndex) : Array[Int] = {
//    index.indices.map {
//      case L4_IntegerConstant(const) => const.toInt
//      case _ : L4_Modulo             => 0
//      case other                     => Logger.warn(s"Unsupported offset found: $other"); 0
//    }
//  }
//
//  def minValuesForAnyIndex(index : L4_Index) : Array[Int] = {
//    index match {
//      case ei : L4_ExpressionIndex => minValuesForExprIndex(ei)
//      case ci : L4_ConstIndex      => ci.indices
//    }
//  }
//
//  def maxValuesForExprIndex(index : L4_ExpressionIndex) : Array[Int] = {
//    index.indices.map {
//      case L4_IntegerConstant(const)             => const.toInt
//      case L4_Modulo(_, L4_IntegerConstant(div)) => (div - 1).toInt
//      case other                                 => Logger.warn(s"Unsupported offset found: $other"); 0
//    }
//  }
//
//  def maxValuesForAnyIndex(index : L4_Index) : Array[Int] = {
//    index match {
//      case ei : L4_ExpressionIndex => maxValuesForExprIndex(ei)
//      case ci : L4_ConstIndex      => ci.indices
//    }
//  }
//
//  def extractMinOffsetArray(numDims : Int, offset : Option[L4_ExpressionIndex]) : Array[Int] = {
//    if (offset.isEmpty)
//      Array.fill(numDims)(0)
//    else
//      minValuesForExprIndex(offset.get)
//  }
//
//  def extractMaxOffsetArray(numDims : Int, offset : Option[L4_ExpressionIndex]) : Array[Int] = {
//    if (offset.isEmpty)
//      Array.fill(numDims)(0)
//    else
//      maxValuesForExprIndex(offset.get)
//  }
//
//  def processReadExtent(field : L4_Field, offset : Option[L4_ExpressionIndex], offset2 : Option[L4_Index] = None) = {
//    // honor offsets in field accesses if present - otherwise assume zero
//    var minOffset = extractMinOffsetArray(field.numDimsGrid, offset)
//    var maxOffset = extractMaxOffsetArray(field.numDimsGrid, offset)
//
//
//    if (offset2.isDefined) {
//      minOffset = (minOffset, minValuesForAnyIndex(offset2.get)).zipped.map(_ + _)
//      maxOffset = (maxOffset, maxValuesForAnyIndex(offset2.get)).zipped.map(_ + _)
//    }
//
//    // take loop extent offsets into account
//    minOffset = (minOffset, beginOffset).zipped.map(_ + _)
//    maxOffset = (maxOffset, endOffset).zipped.map(_ + _)
//
//    // update read extents
//    if (!readExtentMin.contains(field))
//      readExtentMin += (field -> minOffset)
//    else
//      readExtentMin.update(field, (readExtentMin(field), minOffset).zipped.map(math.min))
//
//    if (!readExtentMax.contains(field))
//      readExtentMax += (field -> maxOffset)
//    else
//      readExtentMax.update(field, (readExtentMax(field), maxOffset).zipped.map(math.max))
//  }
//
//  def processWriteExtent(field : L4_Field, offset : Option[L4_ExpressionIndex]) = {
//    // honor offsets in field accesses if present - otherwise assume zero
//    var minOffset = extractMinOffsetArray(field.numDimsGrid, offset)
//    var maxOffset = extractMaxOffsetArray(field.numDimsGrid, offset)
//
//    // take loop extent offsets into account
//    minOffset = (minOffset, beginOffset).zipped.map(_ + _)
//    maxOffset = (maxOffset, endOffset).zipped.map(_ + _)
//
//    // update write extents
//    if (!writeExtentMin.contains(field))
//      writeExtentMin += (field -> minOffset)
//    else
//      writeExtentMin.update(field, (writeExtentMin(field), minOffset).zipped.map(math.min))
//
//    if (!writeExtentMax.contains(field))
//      writeExtentMax += (field -> maxOffset)
//    else
//      writeExtentMax.update(field, (writeExtentMax(field), maxOffset).zipped.map(math.max))
//  }
//
//  override def enter(node : Node) : Unit = {
//    node match {
//      case loop : L4_ForLoop if loop.contraction.isDefined => {
//        // store offsets when entering contracting loops
//
//        val contraction = loop.contraction.get
//        contractionOffsetBegin = contraction.posExt.extractArray.map(_ * loop.number)
//        contractionOffsetEnd = contraction.negExt.getOrElse(contraction.posExt).extractArray.map(_ * loop.number)
//      }
//
//      case loop : L4_LoopOverField => {
//        // store offsets when entering a loop
//        // enable collection mode
//        ignore = false
//
//        val field = loop.field.asInstanceOf[L4_FieldAccess].target
//        val numDims = field.numDimsGrid
//        beginOffset = extractMinOffsetArray(numDims, loop.startOffset)
//        endOffset = extractMaxOffsetArray(numDims, loop.endOffset).map(-1 * _) // invert due to specification in DSL
//
//        // count cell iterations -> increase end offset for each dimension where node discretization is present
//        field.fieldLayout.discretization.toLowerCase match {
//          case "node"   => endOffset = endOffset.map(_ + 1)
//          case "face_x" => endOffset(0) += 1
//          case "face_y" => endOffset(1) += 1
//          case "face_z" => endOffset(2) += 1
//          case "cell"   =>
//          case other    => Logger.warn(s"Encountered unknown localization $other")
//        }
//
//        // account for contracting loops
//        if (!contractionOffsetBegin.isEmpty)
//          beginOffset = (beginOffset, contractionOffsetBegin).zipped.map(_ + _)
//        if (!contractionOffsetEnd.isEmpty)
//          beginOffset = (endOffset, contractionOffsetEnd).zipped.map(_ + _)
//      }
//
//      case L4_Assignment(field : L4_FieldAccess, _, _, cond) => {
//        if (ignore) Logger.warn("Found assignment to field outside kernel")
//
//        // store write access for lhs
//        processWriteExtent(field.target, field.offset)
//
//        // TODO: find a way to ignore recursive match on (lhs) L4_FieldAccess and the wrongfully detected read access
//      }
//
//      case L4_StencilConvolution(stencil, field) => {
//        if (ignore) Logger.warn("Found stencil convolution outside kernel")
//
//        // process each entry (offset) of the stencil
//        for (entry <- stencil.target.entries)
//          processReadExtent(field.target, field.offset, Some(entry.offset))
//
//        // TODO: find a way to ignore recursive match on L4_FieldAccess - issues if (0,0,0) entry is not present
//      }
//
//      case L4_StencilFieldConvolution(op, field) => {
//        if (ignore) Logger.warn("Found stencil field convolution outside kernel")
//
//        // process each entry (offset) of the stencil template
//        for (offset <- op.target.offsets)
//          processReadExtent(field.target, field.offset, Some(offset))
//
//        // process access to stencil coefficients
//        processReadExtent(op.target, op.offset)
//
//        // TODO: find a way to ignore recursive match on L4_FieldAccess - issues if (0,0,0) entry is not present
//      }
//
//      // TODO: other convolutions - or unify convolutions
//      // TODO: model StencilFieldAccesses
//
//      case field : L4_FieldAccess => {
//        if (!ignore)
//          processReadExtent(field.target, field.offset)
//      }
//
//      case _ =>
//    }
//  }
//
//  override def leave(node : Node) : Unit = {
//    node match {
//      case loop : L4_ForLoop if loop.contraction.isDefined => {
//        contractionOffsetBegin = Array()
//        contractionOffsetEnd = Array()
//      }
//
//      case loop : L4_LoopOverField => {
//        // ignore all field accesses outside kernels (eg in advance statements)
//        ignore = true
//        beginOffset = Array[Int]()
//        endOffset = Array[Int]()
//      }
//
//      case _ =>
//    }
//  }
//
//  override def reset() : Unit = {
//    readExtentMin.clear
//    readExtentMax.clear
//    writeExtentMin.clear
//    writeExtentMax.clear
//  }
//}
