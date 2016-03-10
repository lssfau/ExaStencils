package exastencils.core

import java.lang.reflect._
import scala.collection.immutable.Nil

/**
  * Support object used to (deep) clone object instances.
  */
object Duplicate {
  private final val cloner = new com.rits.cloning.Cloner
  cloner.setDumpClonedClasses(Settings.printClonedObjects);

  /**
    * Deep clones the given object instance.
    *
    * @param t The object instance to be cloned.
    * @return The cloned instance.
    */
  def apply[T](t : T) : T = cloner.deepClone(t)

  /**
    * Deep clones the given object instance multiple times.
    *
    * @param t The object instance to be cloned.
    * @param n The number of clones to be created.
    * @return A [[scala.collection.immutable.List]] holding the cloned instances.
    */
  def apply[T](t : T, n : Integer) : List[T] = List.fill(n)(cloner.deepClone(t))

  /**
    * Enable or disable debug output.
    *
    * @param output true if debug output should be printed, otherwise false.
    */
  def debugOutput(output : Boolean) = cloner.setDumpClonedClasses(output)

  def dontClone(t : Class[_]) = cloner.dontClone(t)
  def dontCloneHierarchy(t : Class[_]) = cloner.dontCloneInstanceOf(t)
  def nullInsteadOfClone(t : Class[_]) = cloner.nullInsteadOfClone(t)

  /**
    * Register a class as immutable, which does not need to be cloned.
    *
    * @param t The class to be registered as immutable.
    */
  def registerImmutable(t : Class[_]) = cloner.registerImmutable(t)
  def registerConstant(t : Class[_], field : String) = cloner.registerConstant(t, field)
  def registerConstant(field : Any) = cloner.registerConstant(field)

  // prevent cloning of some immutable objects of the scala library (otherwise something goes boom)
  cloner.registerImmutable(None.getClass)
  cloner.registerImmutable(Nil.getClass)
  cloner.registerImmutable(scala.Array.getClass)
  cloner.registerImmutable(scala.collection.immutable.$colon$colon.getClass)
  cloner.registerImmutable(scala.collection.immutable.Map.getClass)
  cloner.registerImmutable(scala.collection.immutable.HashMap.getClass)
  cloner.registerImmutable(scala.collection.immutable.List.getClass)
  cloner.registerImmutable(scala.collection.immutable.Seq.getClass)
  cloner.registerImmutable(scala.collection.immutable.Set.getClass)
  cloner.registerImmutable(scala.collection.immutable.HashSet.getClass)
  cloner.registerImmutable(scala.Int.getClass)
  cloner.registerImmutable(scala.Double.getClass)
  cloner.registerImmutable(scala.Float.getClass)
}
