package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.field.l2.L2_FieldAccess
import exastencils.prettyprinting.PpStream

/// L2_OperatorFromEquation

object L2_OperatorFromEquation {
  def apply(levels : Option[L2_DeclarationLevelSpecification], entries : List[L2_OperatorFromEqEntry])
  = new L2_OperatorFromEquation(levels, entries.to[ListBuffer])
}

case class L2_OperatorFromEquation(
    var levels : Option[L2_LevelSpecification],
    var entries : ListBuffer[L2_OperatorFromEqEntry]) extends L2_StencilDecl {

  override def name : String = ???
  override def prettyprint(out : PpStream) : Unit = ???

  override def addToKnowledge() = {
    import L2_GenerateStencilFromEquation._

    for (entry <- entries) {
      targetFields = entry.mapping.map(_.unknown.asInstanceOf[L2_FieldAccess])

      process(entry.equation)

      for (rawStencil <- stencils) {
        val stencilName = entry.mapping.find(rawStencil._1 == _.unknown).get.name
        val stencilEntries = rawStencil._2.map(entry => L2_StencilOffsetEntry(Duplicate(entry.offset), Duplicate(entry.coefficient)))

        if (true) {
          val stencil = L2_Stencil(stencilName, L2_LevelSpecification.asSingleLevel(levels), 2, Array(1, 1), stencilEntries.map(_.asStencilMappingEntry))

          //Logger.debug("New stencil:\n" + stencil.printStencilToStr(true))

          L2_StencilCollection.add(stencil)
        } else {
          // inherit localization from target field
          val localization = entry.targetField.asInstanceOf[L2_FieldAccess].target.localization
          val decl = L2_StencilFieldDecl(stencilName, levels, localization, "global", stencilEntries.map(_.asInstanceOf[L2_StencilEntry]))
          decl.addToKnowledge()
        }
      }
    }
  }

  override def progress = ???

  def addDeclarations() = entries.foreach(_.mapping.foreach(m => L2_StencilCollection.addDeclared(m.name, levels)))
}

/// L2_OperatorMapping

case class L2_OperatorMapping(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var unknown : L2_Access) extends L2_Node {

}

/// L2_OperatorFromEqEntry

object L2_OperatorFromEqEntry {
  def apply(targetField : L2_Access, equation : L2_Equation, mapping : List[L2_OperatorMapping]) =
    new L2_OperatorFromEqEntry(targetField, equation, mapping.to[ListBuffer])
}

case class L2_OperatorFromEqEntry(
    var targetField : L2_Access,
    var equation : L2_Equation,
    var mapping : ListBuffer[L2_OperatorMapping]) extends L2_Node {

}
