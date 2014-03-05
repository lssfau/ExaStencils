package exastencils.core

import java.lang.reflect._

object Duplicate {
  val cloner = new com.rits.cloning.Cloner

  def apply[T](t : T) : T = cloner.deepClone(t)

  def debugOutput(output : Boolean) = cloner.setDumpClonedClasses(output)

  def dontClone(t : Class[_]) = cloner.dontClone(t)
  def dontCloneHierarchy(t : Class[_]) = cloner.dontCloneInstanceOf(t)
  def nullInsteadOfClone(t : Class[_]) = cloner.nullInsteadOfClone(t)
  def registerImmutable(t : Class[_]) = cloner.registerImmutable(t)
  def registerConstant(t : Class[_], field : String) = cloner.registerConstant(t, field)
  def registerConstant(field : Any) = cloner.registerConstant(field)
}

