package exastencils.core

import reflect.runtime.universe._

object UniversalSetter {
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