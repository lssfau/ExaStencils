package exastencils.datastructures

import java.lang.reflect._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object Duplicate {
  val cloner = new com.rits.cloning.Cloner
//  cloner.setDumpClonedClasses(true);

  def apply[T](t : T) : T = cloner.deepClone(t)
}

