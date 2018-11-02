package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l4._

/// L4_VariableAccess

trait L4_VariableAccess extends L4_Access {
  def datatype : L4_Datatype
}

/// L4_PlainVariableAccess

case class L4_PlainVariableAccess(var name : String, var datatype : L4_Datatype, var isConst : Boolean) extends L4_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = ProgressLocation(IR_VariableAccess(name, datatype.progress))
}

/// L4_LeveledVariableAccess

case class L4_LeveledVariableAccess(var name : String, var level : Int, var datatype : L4_Datatype, var isConst : Boolean) extends L4_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name << '@' << level
  override def progress = ProgressLocation(IR_VariableAccess(name + "_" + level, datatype.progress))
}

/// L4_ResolveVariableAccesses

object L4_ResolveVariableAccesses extends DefaultStrategy("Resolve value and variable accesses") {
  var declCollector = new L4_VariableDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L4_LevelCollector
  this.register(levelCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L4_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for level in access and decl
      if (declCollector.existsPlain(access.name)) {
        // access to plain variable
        if (access.level.nonEmpty) Logger.warn(s"Level access to un-leveled variable/ value ${ access.name } will be ignored")

        val decl = declCollector.getDeclaration(access.name)
        if (Knowledge.experimental_l4_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L4_PlainVariableAccess(decl.name, decl.datatype, decl.isConst)
      } else {
        // access to leveled variable
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ access.name }")
        }

        val decl = declCollector.getDeclaration(access.name, lvl)
        if (Knowledge.experimental_l4_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L4_LeveledVariableAccess(decl.name, lvl, decl.datatype, decl.isConst)
      }
  })

  this += new Transformation("Remove applicable declarations", {
    case decl : L4_VariableDeclaration if Knowledge.experimental_l4_inlineValueDeclarations && decl.isConst =>
      None // consume declaration
  })
}
