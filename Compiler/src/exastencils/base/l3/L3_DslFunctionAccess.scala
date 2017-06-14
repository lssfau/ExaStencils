package exastencils.base.l3

import exastencils.base.l4.L4_UserFunctionAccess
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

/// L3_PlainDslFunctionAccess

case class L3_PlainDslFunctionAccess(var name : String, var datatype : L3_Datatype) extends L3_PlainFunctionAccess {
  override def progress = L4_UserFunctionAccess(name, datatype.progress)
}

/// L3_LeveledDslFunctionAccess

case class L3_LeveledDslFunctionAccess(var name : String, var level : Int, var datatype : L3_Datatype) extends L3_LeveledFunctionAccess {
  override def progress = L4_UserFunctionAccess(name, Some(level), datatype.progress)
}

/// L3_ResolveDslFunctionAccesses

object L3_ResolveDslFunctionAccesses extends DefaultStrategy("Resolve function accesses") {
  val declCollector = L3_FunctionCollector
  this.register(declCollector)

  val levelCollector = new L3_LevelCollector
  this.register(levelCollector)

  this += new Transformation("Collecting function declarations", PartialFunction.empty)

  this += new Transformation("Resolve function accesses", {
    case access : L3_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for levels in access and decl
      if (declCollector.existsPlain(access.name)) {
        // access to plain function
        if (access.level.nonEmpty) Logger.warn(s"Level access in function call to un-leveled function ${ access.name } will be ignored")
        L3_PlainDslFunctionAccess(access.name, declCollector.getDatatype(access.name))
      } else {
        // access to leveled function
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ access.name }")
        }

        L3_LeveledDslFunctionAccess(access.name, lvl, declCollector.getDatatype(access.name, lvl))
      }
  })
}
