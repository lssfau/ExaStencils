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

package exastencils.runner

import scala.collection.mutable.ListBuffer

/// RunnerConfig

object RunnerConfig {
  val debug = true

  def apply(variabilities : Option[List[Variability]], constraints : Option[List[Constraint]], derivedVals : Option[List[DerivedParameter]]) =
    new RunnerConfig(variabilities.getOrElse(List()).to[ListBuffer], constraints.getOrElse(List()).to[ListBuffer], derivedVals.getOrElse(List()).to[ListBuffer])
}

case class RunnerConfig(var variabilities : ListBuffer[Variability], var constraints : ListBuffer[Constraint], var derivedParams : ListBuffer[DerivedParameter]) {
  def print() : String = {
    var out = ""

    out += "\nVariabilities:\n"
    for (s <- variabilities) out += '\t' + s.print() + '\n'

    out += "\nConstraints:\n"
    for (c <- constraints) out += '\t' + c.print() + '\n'

    out += "\nDerivedParameter:\n"
    for (p <- derivedParams) out += '\t' + p.print() + '\n'

    out
  }

  def generateConfigurations() : ListBuffer[Configuration] = {
    var genSamples = ListBuffer[Configuration]()

    def updateGenSamples(name : String, values : ListBuffer[Any]) : Unit = {
      if (genSamples.isEmpty)
        genSamples = values.map(v => Configuration(ListBuffer(Parameter(name, v))))
      else
        genSamples = genSamples.flatMap(oldList =>
          values.map(v => Configuration(oldList.parameters :+ Parameter(name, v)))
        )
    }

    for (v <- variabilities) v match {
      case VariabilitiesFromList(name, list)     => updateGenSamples(name, list)
      case VariabilitiesFromLambda(name, lambda) => updateGenSamples(name, lambda.eval[Seq[Any]]().to[ListBuffer])
    }

    genSamples
  }

  def setDerivedParams() : Unit = derivedParams.foreach(_.apply())
}
