package exastencils.base.l3

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l3.L3_LevelCollector

/// L3_PlainDslFunctionReference

case class L3_PlainDslFunctionReference(var name : String, var returnType : L3_Datatype) extends L3_PlainFunctionReference {
  override def progress = L4_PlainDslFunctionReference(name, returnType.progress)
}

/// L3_LeveledDslFunctionReference

case class L3_LeveledDslFunctionReference(var name : String, var level : Int, var returnType : L3_Datatype) extends L3_LeveledFunctionReference {
  override def progress = L4_LeveledDslFunctionReference(name, level, returnType.progress)
}

/// L3_ResolveDslFunctionReferences

object L3_ResolveDslFunctionReferences extends DefaultStrategy("Resolve function references") {
  val declCollector = L3_FunctionCollector
  this.register(declCollector)

  val levelCollector = new L3_LevelCollector
  this.register(levelCollector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Collecting function declarations", PartialFunction.empty)

  this += new Transformation("Resolve", {
    case ref : L3_UnresolvedFunctionReference if declCollector.exists(ref.name) =>
      // check for levels in ref and decl
      if (declCollector.existsPlain(ref.name)) {
        // access to plain function
        if (ref.level.nonEmpty) Logger.warn(s"Level access in function call to un-leveled function ${ ref.name } will be ignored")
        L3_PlainDslFunctionReference(ref.name, declCollector.getDatatype(ref.name))
      } else {
        // access to leveled function
        val lvl = {
          if (ref.level.isDefined) ref.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ ref.name }")
        }

        L3_LeveledDslFunctionReference(ref.name, lvl, declCollector.getDatatype(ref.name, lvl))
      }
  })
}
