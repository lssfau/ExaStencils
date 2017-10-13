package exastencils.base.l3

import exastencils.base.l4._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l3._

/// L3_VariableAccess

trait L3_VariableAccess extends L3_Access {
  def datatype : L3_Datatype
}

/// L3_PlainVariableAccess

case class L3_PlainVariableAccess(var name : String, var datatype : L3_Datatype, var isConst : Boolean) extends L3_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = L4_PlainVariableAccess(name, datatype.progress, isConst)
}

/// L3_LeveledVariableAccess

case class L3_LeveledVariableAccess(var name : String, var level : Int, var datatype : L3_Datatype, var isConst : Boolean) extends L3_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name << '@' << level
  override def progress = L4_LeveledVariableAccess(name, level, datatype.progress, isConst)
}

/// L3_ResolveVariableAccesses

object L3_ResolveVariableAccesses extends DefaultStrategy("Resolve value and variable accesses") {
  var declCollector = new L3_VariableDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L3_LevelCollector
  this.register(levelCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L3_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for level in access and decl
      if (declCollector.existsPlain(access.name)) {
        // access to plain variable
        if (access.level.nonEmpty) Logger.warn(s"Level access to un-leveled variable/ value ${ access.name } will be ignored")

        val decl = declCollector.getDeclaration(access.name)
        if (Knowledge.experimental_l3_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L3_PlainVariableAccess(decl.name, decl.datatype, decl.isConst)
      } else {
        // access to leveled variable
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ access.name }")
        }

        val decl = declCollector.getDeclaration(access.name, lvl)
        if (Knowledge.experimental_l3_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L3_LeveledVariableAccess(decl.name, lvl, decl.datatype, decl.isConst)
      }
  })
}
