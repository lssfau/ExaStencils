package exastencils.core

object UniversalSetter {
  def apply[T](obj: AnyRef, ident: String, value: T) = {
    Logger.info("UniversalSetter: Setting " + ident + " to " + value)

    val field = obj.getClass.getDeclaredField(ident)
    val accessible = field.isAccessible
    field.setAccessible(true)

    obj.getClass.getFields.find(_.getName == ident).foreach(_.set(obj, value))
    
    if (field.get(obj).getClass.equals(None.getClass())) {
      // Field is Option[T]
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, Option[T](value))
    } else {
      // field is POSO - set directly
      obj.getClass.getMethods.find(_.getName == ident + "_$eq").get.invoke(obj, value.asInstanceOf[Object])
    }
    
    field.setAccessible(accessible)
  }
}