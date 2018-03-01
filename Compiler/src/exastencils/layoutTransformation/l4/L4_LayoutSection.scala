package exastencils.layoutTransformation.l4

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_MatrixDatatype
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l4.L4_BaseFieldDecl
import exastencils.field.l4.L4_FieldLayoutDecl
import exastencils.layoutTransformation.ir.IR_LayoutTransformationCollection
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_LayoutSection

case class L4_LayoutSection(var statements : ListBuffer[L4_LayoutTransformStatement] = ListBuffer()) extends L4_Node with PrettyPrintable with L4_Progressable {
  override def prettyprint(out : PpStream) : Unit = {
    if (statements.nonEmpty) out << "LayoutTransformations {\n" <<< (statements, "\n") << "\n}"
  }

  override def progress = ProgressLocation(IR_LayoutTransformationCollection(statements.map(_.progress)))
}

/// L4_UnifyLayoutSections

object L4_UnifyLayoutSections extends DefaultStrategy("Unify all layout sections") {
  var unifiedLayoutSection : L4_LayoutSection = null

  override def apply(applyAtNode : Option[Node]) : Unit = {
    super.apply(applyAtNode)

    // reset unifiedLayoutSection for potential subsequent runs
    unifiedLayoutSection = null
  }

  this += new Transformation("Collect and consume layout sections", {
    case layouts : L4_LayoutSection =>
      if (unifiedLayoutSection == null) {
        unifiedLayoutSection = layouts
        layouts
      } else {
        unifiedLayoutSection.statements ++= layouts.statements
        None
      }
  })
}

/// L4_AaddSOAtoAOSTransformation

object L4_AddSoAtoAoSTransformation extends DefaultStrategy("Add SoA to AoS transformation for all fields with matrix or vector element type") {

  override def apply(applyAtNode : Option[Node]) : Unit = {
    val fieldDecls = ArrayBuffer[L4_BaseFieldDecl]()
    val layoutDecls = ArrayBuffer[L4_FieldLayoutDecl]()
    var break = false

    this += new Transformation("collect fields with matrix or vector elements", {
      case n if break                 => // early exit as the transformation is not recursive
        n
      case fDecl : L4_BaseFieldDecl   =>
        fieldDecls += fDecl
        fDecl
      case lDecl : L4_FieldLayoutDecl =>
        lDecl.datatype match {
          case _ : L4_VectorDatatype | _ : L4_MatrixDatatype =>
            layoutDecls += lDecl
          case _                                             =>
        }
        lDecl
      case lSec : L4_LayoutSection    =>
        break = true
        lSec
    }, recursive = false)
    super.apply(applyAtNode)

    if (break) {
      Logger.warn("No layout transformations are created since there already exists at least one LayoutTransformations block")
      return
    }

    val layouts = HashMap[String, HashMap[L4_LevelSpecification, (ArrayBuffer[L4_LeveledIdentifier], L4_GenericTransform)]]()
    for (L4_FieldLayoutDecl(name, levels, dt, _, _) <- layoutDecls) {
      val dim : Int = dt.dimensionality
      val fieldIts = (0 until Knowledge.dimensionality).view.map(i => L4_PlainVariableAccess("i" + i, L4_IntegerDatatype, true))
      val matIts = dim match {
        case 1 => List(L4_PlainVariableAccess("v", L4_IntegerDatatype, true)).view
        case d => (0 until d).view.map(i => L4_PlainVariableAccess("m" + i, L4_IntegerDatatype, true))
      }
      val its = (fieldIts ++ matIts).toArray
      val trafo = (matIts ++ fieldIts).toArray[L4_Expression]
      val map = layouts.getOrElseUpdate(name, HashMap())
      val buffer = ArrayBuffer[L4_LeveledIdentifier]()
      map.put(levels.getOrElse(L4_AllLevels), (buffer, L4_GenericTransform(buffer, its, L4_ExpressionIndex(trafo))))
    }

    for (L4_BaseFieldDecl(name, levelsOpt, _, layout, _, _) <- fieldDecls)
      for (layoutLevels <- layouts.get(layout.name)) { // option
        val levels = levelsOpt.getOrElse(L4_AllLevels)
        val trafo = layoutLevels.getOrElse(levels, layoutLevels.getOrElse(L4_AllLevels, null))
        if (trafo != null)
          trafo._1 += L4_LeveledIdentifier(name, Some(Duplicate(levels)))
        else
          Logger.warn(s"creating a default SoA to AoS layout transformation for field $name@${ levels.prettyprint() } not possible " +
            s"(currently the level flag for the layout must either be '@all' or exactly match the one from the field declaration: ${ levels.prettyprint() })")
      }

    val layoutSec = L4_LayoutSection()
    for ((_, map) <- layouts)
      for ((_, (_, trafoStmt)) <- map)
        if (!trafoStmt.fields.isEmpty)
          layoutSec.statements += trafoStmt

    val nodes = ExaRootNode.l4_root.nodes
    nodes.insert(nodes.lastIndexWhere(_.isInstanceOf[L4_BaseFieldDecl]) + 1, layoutSec)
  }
}
