package exastencils.runner

import exastencils.core.UniversalSetter
import exastencils.util.ResolveConfigCollection

case class Parameter(var name : String, var value : Any, var configCollection : AnyRef = null) {
  if (null == configCollection)
    configCollection = ResolveConfigCollection(name)

  def apply() : Unit = UniversalSetter.apply(configCollection, name, value)
}
