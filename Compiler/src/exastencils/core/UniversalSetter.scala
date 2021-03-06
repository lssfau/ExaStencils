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

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

import exastencils.logger._

/**
  * A utility object to set values of any entity by just specifying the instance, the name of the member and the value.
  *
  */
object UniversalSetter {
  /**
    * Sets the value of an entity's member.
    *
    * @param obj   The instance of the entity whose member is to be set.
    * @param ident The name of the member to be set.
    * @param value The new value of the member.
    */
  def apply[T](obj : AnyRef, ident : String, value : T) : Unit = {
    Logger.info("UniversalSetter: Setting " + ident + " to " + value)

    val field = obj.getClass.getDeclaredField(ident)
    val accessible = field.isAccessible
    field.setAccessible(true)

    if (field.get(obj).getClass == None.getClass)      // Field is Option[T]
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, Option[T](value))
    else if (field.get(obj).getClass == ListBuffer().getClass && value.getClass != ListBuffer().getClass) // Field is a ListBuffer[_], but provided value is not
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, ListBuffer(value))
    else // field is POSO - set directly
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, value.asInstanceOf[Object])

    field.setAccessible(accessible)
  }

  /**
    * Adds the value to an entity's list member.
    *
    * @param obj   The instance of the entity.
    * @param ident The name of the member.
    * @param value The additional value of the member.
    */
  def addToListBuffer[T](obj : AnyRef, ident : String, value : T) : Unit = {
    Logger.info("UniversalSetter: Adding " + value + " to " + ident)

    val field = obj.getClass.getDeclaredField(ident)
    val accessible = field.isAccessible
    field.setAccessible(true)
    val lb = obj.getClass.getMethods.find(_.getName == ident).get.invoke(obj).asInstanceOf[ListBuffer[T]]
    lb.+=(value)
    field.setAccessible(accessible)
  }

  /**
    * Converts the new value into the matching type and sets the value of an entity's member.
    *
    * This methods converts the value into a suitable type (e.g., by parsing a string into Int or Float) before setting the new value.
    *
    * @param obj   The instance of the entity whose member is to be set.
    * @param ident The name of the member to be set.
    * @param value The new value of the member.
    */
  def withConversion[T : TypeTag](obj : AnyRef, ident : String, value : T) : Unit = {
    Logger.info("UniversalSetter: Setting (with conversion) " + ident + " to " + value)

    val field = obj.getClass.getDeclaredField(ident)
    val fieldType = field.getGenericType

    value match {
      case s : String =>
        if (fieldType == classOf[Integer]) { return apply(obj, ident, s.toInt) }
        else if (fieldType == classOf[Float]) { return apply(obj, ident, s.toFloat) }
        else if (fieldType == classOf[Double]) { return apply(obj, ident, s.toDouble) }
      case _          =>
    }
    apply(obj, ident, value)
  }
}
