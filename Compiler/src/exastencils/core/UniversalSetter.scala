package exastencils.core

import reflect.runtime.universe._

/**
  * A utility object to set values of any entity by just specifying the instance, the name of the member and the value.
  *
  */
object UniversalSetter {
  /**
    * Sets the value of an entity's member.
    *
    * @param obj The instance of the entity whose member is to be set.
    * @param ident The name of the member to be set.
    * @param value The new value of the member.
    */
  def apply[T](obj : AnyRef, ident : String, value : T) : Unit = {
    Logger.info("UniversalSetter: Setting " + ident + " to " + value)

    val field = obj.getClass.getDeclaredField(ident)
    val accessible = field.isAccessible
    field.setAccessible(true)

    //obj.getClass.getFields.find(_.getName == ident).foreach(_.set(obj, value))

    if (field.get(obj).getClass.equals(None.getClass())) {
      // Field is Option[T]
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, Option[T](value))
    } else {
      // field is POSO - set directly
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, value.asInstanceOf[Object])
    }

    field.setAccessible(accessible)
  }

  /**
    * Converts the new value into the matching type and sets the value of an entity's member.
    *
    * This methods converts the value into a suitable type (e.g., by parsing a string into Int or Float) before setting the new value.
    *
    * @param obj The instance of the entity whose member is to be set.
    * @param ident The name of the member to be set.
    * @param value The new value of the member.
    */
  def withConversion[T : TypeTag](obj : AnyRef, ident : String, value : T) : Unit = {
    Logger.info("UniversalSetter: Setting (with conversion) " + ident + " to " + value)

    val field = obj.getClass.getDeclaredField(ident)
    val fieldType = field.getGenericType()

    value match {
      case s : String => {
        if (fieldType == classOf[Integer]) { return apply(obj, ident, s.toInt) }
        else if (fieldType == classOf[Float]) { return apply(obj, ident, s.toFloat) }
        else if (fieldType == classOf[Double]) { return apply(obj, ident, s.toDouble) }
      }
      case _ =>
    }
    apply(obj, ident, value)
  }
}