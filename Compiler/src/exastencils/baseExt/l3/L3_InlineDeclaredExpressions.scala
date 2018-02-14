package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l3._

/// L3_InlineDeclaredExpressions

object L3_InlineDeclaredExpressions extends DefaultStrategy("Inline accesses to declared expressions") {
  var declCollector = new L3_ExpressionDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L3_LevelCollector
  this.register(levelCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L3_UnresolvedAccess if declCollector.exists(access.name) =>
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

      val wrappedExpr = L3_ExpressionStatement(Duplicate(decl.expr))

      // inherit modifiers
      if (access.offset.isDefined) {
        L3_OffsetAllApplicable.offset = access.offset.get
        L3_OffsetAllApplicable.applyStandalone(wrappedExpr)
      }
      if (access.dirAccess.isDefined)
        Logger.warn(s"Ignoring direction access when inlining expression ${ access.name }")
      if (access.slot.isDefined)
        Logger.warn(s"Ignoring slot access when inlining expression ${ access.name }")

      wrappedExpr.expression
  })

  this += new Transformation("Remove applicable declarations", {
    case _ : L3_ExpressionDeclaration =>
      None // consume declaration
  })
}
