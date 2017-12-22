package exastencils.base.l2

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l2._

/// L2_VariableAccess

trait L2_VariableAccess extends L2_Access

/// L2_PlainVariableAccess

case class L2_PlainVariableAccess(var name : String, var datatype : L2_Datatype, var isConst : Boolean) extends L2_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = ProgressLocation(L3_PlainVariableAccess(name, datatype.progress, isConst))
}

/// L2_LeveledVariableAccess

case class L2_LeveledVariableAccess(var name : String, var level : Int, var datatype : L2_Datatype, var isConst : Boolean) extends L2_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name << '@' << level
  override def progress = ProgressLocation(L3_LeveledVariableAccess(name, level, datatype.progress, isConst))
}

/// L2_ResolveVariableAccesses

object L2_ResolveVariableAccesses extends DefaultStrategy("Resolve value and variable accesses") {
  var declCollector = new L2_VariableDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L2_LevelCollector
  this.register(levelCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L2_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for level in access and decl
      if (declCollector.existsPlain(access.name)) {
        // access to plain variable
        if (access.level.nonEmpty) Logger.warn(s"Level access to un-leveled variable/ value ${ access.name } will be ignored")

        val decl = declCollector.getDeclaration(access.name)
        if (Knowledge.experimental_l2_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L2_PlainVariableAccess(decl.name, decl.datatype, decl.isConst)
      } else {
        // access to leveled variable
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ access.name }")
        }

        val decl = declCollector.getDeclaration(access.name, lvl)
        if (Knowledge.experimental_l2_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L2_LeveledVariableAccess(decl.name, lvl, decl.datatype, decl.isConst)
      }
  })
}
