package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.logger._
import exastencils.prettyprinting._

/// L2_StencilDecl

abstract class L2_StencilDecl extends L2_LeveledKnowledgeDecl {
  def name : String
  override def progress = { Logger.error(s"Trying to progress l2 stencil declaration for stencil $name; this is not supported") }
}

/// L2_BaseStencilDecl

object L2_BaseStencilDecl {
  def apply(name : String, levels : Option[L2_LevelSpecification], entries : List[L2_StencilEntry]) =
    new L2_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L2_BaseStencilDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var entries : ListBuffer[L2_StencilEntry]) extends L2_StencilDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"
}

/// L2_StencilFromExpression

case class L2_StencilFromExpression(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var expression : L2_Expression) extends L2_StencilDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"
}

/// L2_UnfoldStencilDeclarations

object L2_UnfoldStencilDeclarations extends DefaultStrategy("Unfold L2 stencil declarations") {
  this += Transformation("Process new stencils", {
    case decl : L2_BaseStencilDecl       => L2_LeveledKnowledgeDecl.unfoldDecl(decl)
    case decl : L2_StencilFromExpression => L2_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}

/// L2_PrepareStencilDeclaration

object L2_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for L2 stencils") {
  this += Transformation("Process new stencils", {
    case decl : L2_BaseStencilDecl =>
      L2_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement

    case decl : L2_StencilFromExpression =>
      L2_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L2_ProcessStencilDeclaration

object L2_ProcessStencilDeclarations extends DefaultStrategy("Integrate L2 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : L2_BaseStencilDecl if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      // TODO: check stencil - numDims for entries, stride, etc.
      val numDims = decl.entries.map(_.numDims).max
      val colStride = decl.entries.map(_.colStride).head

      val level = L2_LevelSpecification.asSingleLevel(decl.levels)
      L2_StencilCollection.add(L2_Stencil(decl.name, level, numDims, colStride, decl.entries.map(_.asStencilMappingEntry)))

      None // consume declaration statement

    case decl : L2_StencilFromExpression if !L2_FutureKnowledgeAccess.existsInStmt(decl) =>
      // resolve accesses to already processed stencils if possible
      decl.expression match {
        case L2_FunctionCall(fctAccess : L2_Access, args) if "kron" == fctAccess.name =>
          args match {
            case ListBuffer(left : L2_StencilAccess, right : L2_StencilAccess) =>
              val stencil = left.target.kron(right.target)
              stencil.name = decl.name
              L2_StencilCollection.add(stencil)

              None // consume declaration statement

            case _ =>
              // TODO - WIP
              Logger.warn(decl.expression)
              decl
          }

        case _ =>
          // TODO - WIP
          Logger.warn(decl.expression)
          decl
      }
  })
}
