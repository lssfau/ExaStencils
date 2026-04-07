package exastencils.waLBerla.l4.refinement

import exastencils.base.ir.IR_Statement
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.knowledge.l4.L4_KnowledgeDecl
import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_AABB
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementSelection

/// L4_WaLBerlaRefinementSelection

case class L4_WaLBerlaRefinementSelection(name : String, aabb : L4_AABB, refinementTargetLevel : Int) extends L4_KnowledgeObject[IR_WaLBerlaRefinementSelection] {
  override def progressImpl() : IR_WaLBerlaRefinementSelection = IR_WaLBerlaRefinementSelection(name, aabb.progress, refinementTargetLevel)

  override def prettyprintDecl(out : PpStream) : Unit =
    out << "waLBerla RefinementSelection " << name << "< " << aabb << " >" << " with level " << refinementTargetLevel
}

/// L4_WaLBerlaRefinementSelectionDecl

case class L4_WaLBerlaRefinementSelectionDecl(name : String, lower : Array[Double], upper : Array[Double], refinementTargetLevel : Int) extends L4_KnowledgeDecl {

  if (refinementTargetLevel >= Knowledge.waLBerla_refinementLevels)
    Logger.error(s"Target refinement level of selection $name exceeds 'Knowledge.waLBerla_refinementLevels'")

  override def prettyprint(out : PpStream) =
    out << "waLBerla RefinementSelection " << name << "< [" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "] >" << " with level " << refinementTargetLevel
  override def addToKnowledge() : Unit = L4_WaLBerlaRefinementSelectionCollection.add(L4_WaLBerlaRefinementSelection(name, L4_AABB(lower, upper), refinementTargetLevel))

  override def progress : IR_Statement = Logger.error(s"Trying to progress L4 refinement selection declaration for $name; this is not supported")
}

/// L4_PrepareRefinementSelectionDeclarations

object L4_PrepareRefinementSelectionDeclarations extends DefaultStrategy("Prepare knowledge for L4 refinement selection decls") {
  this += Transformation("Process new refinement selections", {
    case decl : L4_WaLBerlaRefinementSelectionDecl =>
      L4_WaLBerlaRefinementSelectionCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L4_ProcessRefinementSelectionDeclarations

object L4_ProcessRefinementSelectionDeclarations extends DefaultStrategy("Integrate L4 refinement selection decls with knowledge") {
  this += Transformation("Process new refinement selections", {
    case decl : L4_WaLBerlaRefinementSelectionDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}