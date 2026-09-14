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

import scala.collection.mutable.HashSet

import exastencils.logger.Logger

/// LegacyParameters
///
/// Mixed into configuration collections to keep renamed parameters accessible under their former name
/// Legacy names are resolved to their current counterpart wherever parameters are set or looked up by name

trait LegacyParameters {
  /// maps deprecated parameter names to their current counterpart
  def legacyParameters : Map[String, String] = Map()

  /// deprecated names already reported -> each one is only warned about once
  private val reported = HashSet[String]()

  def resolveLegacyParameter(ident : String) : String = legacyParameters.get(ident) match {
    case Some(current) =>
      if (reported.add(ident))
        Logger.warn(s"Parameter $ident is deprecated; use $current instead")
      current
    case None          => ident
  }
}
