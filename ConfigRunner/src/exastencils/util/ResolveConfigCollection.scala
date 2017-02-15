package exastencils.util

import exastencils.config._
import exastencils.logger.Logger

/// ResolveConfigCollection

object ResolveConfigCollection {
  def apply(name : String) : AnyRef = {
    var ret : AnyRef = null

    for (target <- List(Knowledge, Settings, Platform)) {
      if (target.getClass.getMethods.exists(_.getName == name)) {
        if (null != ret) Logger.warn(s"Parameter $name is defined in multiple config collections")
        ret = target
      }
    }

    if (null == ret) Logger.warn(s"Trying to sample undefined parameter $name")

    ret
  }
}
