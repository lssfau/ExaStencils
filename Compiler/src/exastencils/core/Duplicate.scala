package exastencils.core

import scala.collection.immutable.Nil

import exastencils.config.Settings
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.logger.Logger

object Duplicate {
  private val cloner = new com.rits.cloning.Cloner
  cloner.setDumpClonedClasses(Settings.printClonedObjects)
  private val forceCloner = new com.rits.cloning.Cloner
  forceCloner.setDumpClonedClasses(Settings.printClonedObjects)

  val debug = true

  def apply[T](t : T) : T = {
    val cloned = cloner.deepClone(t)

    // check for objects that are not cloned
    if (debug)
      t match {
        // TODO: more generic way
        case _ : L2_KnowledgeObject[_] => Logger.warn("Fruitless call to Duplicate.apply for instance of type " + t.getClass)
        case _ : L3_KnowledgeObject[_] => Logger.warn("Fruitless call to Duplicate.apply for instance of type " + t.getClass)
        case _ : L4_KnowledgeObject[_] => Logger.warn("Fruitless call to Duplicate.apply for instance of type " + t.getClass)
        case _ : IR_KnowledgeObject    => Logger.warn("Fruitless call to Duplicate.apply for instance of type " + t.getClass)
        case _                         =>
      }

    cloned
  }

  def forceClone[T](t : T) : T = {
    forceCloner.deepClone(t)
  }

  def debugOutput(output : Boolean) = cloner.setDumpClonedClasses(output)

  def dontClone(t : Class[_]) = cloner.dontClone(t)
  def dontCloneHierarchy(t : Class[_]) = cloner.dontCloneInstanceOf(t)
  def nullInsteadOfClone(t : Class[_]) = cloner.nullInsteadOfClone(t)
  def registerImmutable(t : Class[_]) = cloner.registerImmutable(t)
  def registerConstant(field : Any) = cloner.registerConstant(field)
  // the following is hard/impossible to deal with in willBeCloned, but since it is not used/required yet...
  // def registerConstant(t : Class[_], field : String) : Unit = cloner.registerConstant(t, field)

  def clonable(o : AnyRef) : Boolean = {
    o ne cloner.shallowClone(o)
  }

  // prevent cloning of some immutable objects/classes of the scala library (otherwise something goes boom)
  // Note: do ONLY register classes as immutable, whose children/attributes are immutable, too (recursively)
  //       e.g., immutable.List must therefore be copied
  List(cloner, forceCloner).foreach(
    c => {
      c.registerConstant(None)
      c.registerConstant(Nil)
      c.registerConstant(scala.Array)
      c.registerImmutable(classOf[scala.Int])
      c.registerImmutable(classOf[scala.Double])
      c.registerImmutable(classOf[scala.Float])
    })
}
