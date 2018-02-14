package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l4._

/// L4_InlineDeclaredExpressions

object L4_InlineDeclaredExpressions extends DefaultStrategy("Inline accesses to declared expressions") {
  var declCollector = new L4_ExpressionDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L4_LevelCollector
  this.register(levelCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L4_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for level in access and decl
      val decl =
        if (declCollector.existsPlain(access.name)) {
          // access to plain variable
          if (access.level.nonEmpty) Logger.warn(s"Level access to un-leveled expression ${ access.name } will be ignored")

          declCollector.getDeclaration(access.name)
        } else {
          // access to leveled variable
          val lvl = {
            if (access.level.isDefined) access.level.get.resolveLevel
            else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
            else Logger.error(s"Missing level for calling of ${ access.name }")
          }

          declCollector.getDeclaration(access.name, lvl)
        }

      val wrappedExpr = L4_ExpressionStatement(Duplicate(decl.expr))

      // inherit modifiers
      if (access.offset.isDefined) {
        L4_OffsetAllApplicable.offset = access.offset.get
        L4_OffsetAllApplicable.applyStandalone(wrappedExpr)
      }
      if (access.dirAccess.isDefined)
        Logger.warn(s"Ignoring direction access when inlining expression ${ access.name }")
      if (access.slot.isDefined)
        Logger.warn(s"Ignoring slot access when inlining expression ${ access.name }")

      wrappedExpr.expression
  })

  this += new Transformation("Remove applicable declarations", {
    case _ : L4_ExpressionDeclaration =>
      None // consume declaration
  })
}
