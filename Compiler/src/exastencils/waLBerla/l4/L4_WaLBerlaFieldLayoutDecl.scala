package exastencils.waLBerla.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_Datatype
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.config.Knowledge
import exastencils.field.l4.L4_FieldLayoutDeclLike
import exastencils.field.l4.L4_FieldLayoutOption
import exastencils.grid.l4.L4_AtCellCenter
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

case class L4_WaLBerlaFieldLayoutDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var datatype : L4_Datatype,
    var waLBerlaLayout : String,
    var options : ListBuffer[L4_FieldLayoutOption]
) extends L4_LeveledKnowledgeDecl with L4_FieldLayoutDeclLike {

  def localization = L4_AtCellCenter

  override def prettyprint(out : PpStream) : Unit = {
    out << "waLBerla Layout "  << name << " < "  << datatype << ", \"" << waLBerlaLayout << "\" > "
    if (levels.isDefined) out << '@' << levels.get
    out << " {\n" <<< (options, "\n") << "\n}"
  }

  def composeLayout(level : Int) : L4_WaLBerlaFieldLayout = {
    def numDimsGrid = Knowledge.dimensionality

    val numGhost = evalFieldLayoutValue("ghostLayers")
    val numDup = evalFieldLayoutValue("duplicateLayers")

    // for lower level exa-internal counterparts
    val innerPointsOpt = options.find(_.name == "innerPoints")
    val innerPoints = if (innerPointsOpt.isDefined)
      innerPointsOpt.get.value
    else
      L4_ConstIndex(Array.fill(numDimsGrid)(1))

    // compile final layout
    L4_WaLBerlaFieldLayout(
      name, level, numDimsGrid,
      datatype,
      waLBerlaLayout,
      numGhost,
      evalFieldLayoutBoolean("ghostLayers"),
      numDup,
      evalFieldLayoutBoolean("duplicateLayers"),
      innerPoints)
  }

  override def addToKnowledge() : Unit = {
    val level = levels.get.resolveLevel
    L4_WaLBerlaFieldLayoutCollection.add(composeLayout(level))
  }

  override def progress = Logger.error(s"Trying to progress l4 waLBerla field layout declaration for $name; this is not supported")
}
