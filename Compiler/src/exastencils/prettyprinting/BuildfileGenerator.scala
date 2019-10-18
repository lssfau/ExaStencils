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

package exastencils.prettyprinting

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger

object BuildfileGenerator {
  def parseGenerators(generators : ListBuffer[String]) : ListBuffer[BuildfileGenerator] = {
    val buildfileGeneratorMap = Map(
      "CMakeGenerator" -> CMakeGenerator,
      "MakefileGenerator" -> MakefileGenerator,
      "ProjectfileGenerator" -> ProjectfileGenerator)

    // check for invalid generators
    for (generator <- generators)
      if (!buildfileGeneratorMap.contains(generator)) {
        Logger.warn("Ignoring invalid buildfile generator: " + generator)
        generators -= generator
      }

    // default to MakefileGenerator in case of empty lists
    // TODO: this should not be necessary - users should be allowed to simply generate source code files
    if (generators.isEmpty) {
      Logger.warn("No buildfile generator specified; defaulting to MakefileGenerator")
      generators += "MakefileGenerator"
    }

    generators.distinct.map(buildfileGeneratorMap(_))
  }
}

trait BuildfileGenerator {
  def write() : Unit
}
