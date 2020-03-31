//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.core

import scala.collection.immutable.Nil
import scala.collection.mutable

import java.util

import com.rits.cloning.IDeepCloner
import exastencils.config.Settings
import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.logger.Logger

object Duplicate {
  private val cloner = new com.rits.cloning.Cloner
  cloner.setDumpClonedClasses(Settings.printClonedObjects)

  val constants : mutable.HashSet[Any] = mutable.HashSet()

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

  /**
    * Enable or disable debug output.
    *
    * @param output true if debug output should be printed, otherwise false.
    */
  def debugOutput(output : Boolean) = cloner.setDumpClonedClasses(output)

  def dontClone(t : Class[_]) = {
    cloner.dontClone(t)
  }
  def dontCloneHierarchy(t : Class[_]) = {
    cloner.dontCloneInstanceOf(t)
  }
  def nullInsteadOfClone(t : Class[_]) = {
    cloner.nullInsteadOfClone(t)
  }

  /**
    * Register a class as immutable, which does not need to be cloned.
    *
    * @param t The class to be registered as immutable.
    */
  def registerImmutable(t : Class[_]) = {
    cloner.registerImmutable(t)
  }
  /**
    * Register a constant, which must not be cloned. (Even forceClone(..) is not allowed to clone these.)
    *
    * @param field The object to be registered as constant.
    */
  def registerConstant(field : Any) = {
    constants += field
    cloner.registerConstant(field)
  }
  // the following is hard/impossible to deal with in willBeCloned, but since it is not used/required yet...
  // def registerConstant(t : Class[_], field : String) : Unit = cloner.registerConstant(t, field)

  // prevent cloning of some immutable objects/classes of the scala library (otherwise something goes boom)
  // Note: do ONLY register classes as immutable, whose children/attributes are immutable, too (recursively)
  //       e.g., immutable.List must therefore be copied
  cloner.registerConstant(None)
  cloner.registerConstant(Nil)
  cloner.registerConstant(scala.Array)
  cloner.registerImmutable(classOf[scala.Int])
  cloner.registerImmutable(classOf[scala.Long])
  cloner.registerImmutable(classOf[scala.Double])
  cloner.registerImmutable(classOf[scala.Float])

  class FastClonerListBuffer extends com.rits.cloning.IFastCloner {
    override def clone(o : scala.Any, iDeepCloner : IDeepCloner, map : util.Map[AnyRef, AnyRef]) : AnyRef = {
      return o.asInstanceOf[scala.collection.mutable.ListBuffer[_]].map(iDeepCloner.deepClone(_, map))
    }
  }

  class FastClonerArrayBuffer extends com.rits.cloning.IFastCloner {
    override def clone(o : scala.Any, iDeepCloner : IDeepCloner, map : util.Map[AnyRef, AnyRef]) : AnyRef = {
      return o.asInstanceOf[scala.collection.mutable.ArrayBuffer[_]].map(iDeepCloner.deepClone(_, map))
    }
  }

  class FastClonerArray extends com.rits.cloning.IFastCloner {
    override def clone(o : scala.Any, iDeepCloner : IDeepCloner, map : util.Map[AnyRef, AnyRef]) : AnyRef = {
      return o.asInstanceOf[scala.Array[_]].map(iDeepCloner.deepClone(_, map))
    }
  }

  // Do not add fast cloners to multiplicationCloner
  List(cloner).foreach(c => {
    c.registerFastCloner(classOf[scala.collection.mutable.ListBuffer[_]], new FastClonerListBuffer())
    c.registerFastCloner(classOf[scala.collection.mutable.ArrayBuffer[_]], new FastClonerArrayBuffer())
    c.registerFastCloner(classOf[scala.Array[_]], new FastClonerArray())
  })
}
