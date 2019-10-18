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
