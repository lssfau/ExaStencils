package exastencils.base.l4

import exastencils.base.ir._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

/// L4_PlainDslFunctionAccess

case class L4_PlainDslFunctionAccess(var name : String, var datatype : L4_Datatype) extends L4_PlainFunctionAccess {
  override def progress = IR_PlainDslFunctionAccess(name, datatype.progress)
}

/// L4_LeveledDslFunctionAccess

case class L4_LeveledDslFunctionAccess(var name : String, var level : Int, var datatype : L4_Datatype) extends L4_LeveledFunctionAccess {
  override def progress = IR_LeveledDslFunctionAccess(name, level, datatype.progress)
}

/// L4_ResolveDslFunctionAccesses

object L4_ResolveDslFunctionAccesses extends DefaultStrategy("Resolve function accesses") {
  val declCollector = L4_FunctionCollector
  this.register(declCollector)

  val levelCollector = new L4_LevelCollector
  this.register(levelCollector)

  this += new Transformation("Collecting function declarations", PartialFunction.empty)

  this += new Transformation("Resolve function accesses", {
    case access : L4_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for levels in access and decl
      if (declCollector.existsPlain(access.name)) {
        // access to plain function
        if (access.level.nonEmpty) Logger.warn(s"Level access in function call to un-leveled function ${ access.name } will be ignored")
        L4_PlainDslFunctionAccess(access.name, declCollector.getDatatype(access.name))
      } else {
        // access to leveled function
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ access.name }")
        }

        L4_LeveledDslFunctionAccess(access.name, lvl, declCollector.getDatatype(access.name, lvl))
      }
  })
}
