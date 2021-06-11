package exastencils.waLBerla.l4

import exastencils.base.ExaRootNode
import exastencils.base.l4.L4_Access
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.base.l4.L4_LevelSpecification
import exastencils.baseExt.l4.L4_MatShape
import exastencils.field.l4.L4_FieldDecl
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

case class L4_WaLBerlaFieldDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var fieldLayout : L4_Access,
    var matShape : Option[L4_MatShape] = None
) extends L4_FieldDecl {


  override def prettyprint(out : PpStream) : Unit = {
    out << "waLBerla Field " << name << "< " << fieldLayout.name << ">"
    if (levels.isDefined) out << '@' << levels.get
    if (matShape.isDefined) out << matShape.get.toString()
  }

  override def addToKnowledge() : Unit = {
    val index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    def lvlListFromDecl(lvls : Option[L4_LevelSpecification]) = L4_LevelSpecification.extractLevelListDefEmpty(lvls)
    val lvl = levels.get.resolveLevel
    var maxLvl = if (lvlListFromDecl(levels).isEmpty) -1 else lvlListFromDecl(levels).max

    // TODO maybe implement a collector for this
    ExaRootNode.l4_root.nodes.foreach {
      case f : L4_WaLBerlaField if name == f.name =>
        if (f.level > maxLvl)
          maxLvl = f.level
      case fDecl : L4_WaLBerlaFieldDecl if name == fDecl.name =>
        val lvlList = lvlListFromDecl(fDecl.levels)
        if (lvlList.nonEmpty && lvlList.max > maxLvl)
          maxLvl = lvlList.max
      case _ =>
    }

    if (maxLvl == -1) Logger.error("L4_WaLBerlaFieldDecl: Could not find max level")

    val wbField = L4_WaLBerlaField(name, lvl, maxLvl, index, fieldLayout.asInstanceOf[L4_WaLBerlaFieldLayoutAccess].target, matShape)
    L4_WaLBerlaFieldCollection.add(wbField)
  }
}