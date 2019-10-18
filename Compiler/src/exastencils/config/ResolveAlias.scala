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

package exastencils.config

import scala.collection.mutable.ListBuffer

import exastencils.core.UniversalSetter
import exastencils.logger.Logger

/// ResolveAlias

object ResolveAlias {
  val configs = List(Knowledge, Settings, Platform)
  var changed = false

  def processString(toProcess : String) : String = {
    if (toProcess.contains('$')) {
      var inVar = false
      var varName = ""
      var output = ""

      // process char by char
      for (c <- toProcess) c match {
        case '$' if !inVar => inVar = true
        case '$' if inVar  =>
          var found = false
          for (target <- configs)
            if (!found && target.getClass.getMethods.exists(_.getName == varName)) {
              found = true
              output += target.getClass.getMethods.find(_.getName == varName).get.invoke(target)
            }

          if (!found) {
            output += "$" + varName + "$"
            Logger.warn(s"Trying to replace undefined parameter $varName; ignored")
          }

          inVar = false
          varName = ""

        case _ if inVar  => varName += c
        case _ if !inVar => output += c
      }

      if (inVar) // ignore unclosed aliases
        output += "$" + varName

      changed |= output != toProcess

      output
    } else toProcess // nothing to do
  }

  def apply() : Unit = {
    // check all listed config collections
    for (config <- configs) {
      // support recursive replacement -> loop until nothing changes any more
      changed = true
      while (changed) {
        changed = false

        // target all member variables with type String (return type of getter is string and setter function exists)
        for (m <- config.getClass.getMethods)
          if (m.getReturnType == "".getClass && config.getClass.getMethods.exists(_.getName == m.getName + "_$eq")) {
            // get name and value of current parameter
            val paramName = m.getName
            val toProcess = m.invoke(config).asInstanceOf[String]

            val processed = processString(toProcess)

            if (toProcess != processed) {
              // write back updated parameter value
              Logger.debug(s"Setting $paramName to $processed")
              UniversalSetter(config, paramName, processed)
            }
          }

        // target all member variables with type ListBuffer[String] (return type of getter is string and setter function exists)
        // note that the element type of a ListBuffer must be checked inside
        for (m <- config.getClass.getMethods)
          if (m.getReturnType == ListBuffer().getClass && config.getClass.getMethods.exists(_.getName == m.getName + "_$eq")) {
            // get name and value of current parameter
            val paramName = m.getName
            val toProcess = m.invoke(config).asInstanceOf[ListBuffer[_]]

            // check for element type
            if (!toProcess.isEmpty && toProcess.head.isInstanceOf[String]) {
              val processed = toProcess.asInstanceOf[ListBuffer[String]].map(processString)

              if (toProcess != processed) {
                // write back updated parameter value
                Logger.debug(s"Setting $paramName to $processed")
                UniversalSetter(config, paramName, processed)
              }
            }
          }
      }
    }
  }
}
