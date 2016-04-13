package exastencils.core

import scala.collection.immutable.Nil

object Duplicate {
  val cloner = new com.rits.cloning.Cloner
  cloner.setDumpClonedClasses(Settings.printClonedObjects);

  def apply[T](t : T) : T = cloner.deepClone(t)

  def debugOutput(output : Boolean) = cloner.setDumpClonedClasses(output)

  def dontClone(t : Class[_]) = cloner.dontClone(t)
  def dontCloneHierarchy(t : Class[_]) = cloner.dontCloneInstanceOf(t)
  def nullInsteadOfClone(t : Class[_]) = cloner.nullInsteadOfClone(t)
  def registerImmutable(t : Class[_]) = cloner.registerImmutable(t)
  def registerConstant(field : Any) = cloner.registerConstant(field)
  // the following is hard/impossible to deal with in willBeCloned, but since it is not used/required yet...
  // def registerConstant(t : Class[_], field : String) : Unit = cloner.registerConstant(t, field)

  def clonable(o : AnyRef) : Boolean = {
    return o ne cloner.shallowClone(o)
  }

  // prevent cloning of some immutable objects/classes of the scala library (otherwise something goes boom)
  // Note: do ONLY register classes as immutable, whose children/attributes are immutable, too (recursively)
  //       e.g., immutable.List must therefore be copied
  this.registerConstant(None)
  this.registerConstant(Nil)
  this.registerConstant(scala.Array)
  this.registerImmutable(classOf[scala.Int])
  this.registerImmutable(classOf[scala.Double])
  this.registerImmutable(classOf[scala.Float])
}
