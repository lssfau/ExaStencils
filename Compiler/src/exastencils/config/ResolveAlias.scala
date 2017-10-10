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
      changed = true
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

          if (!found) Logger.warn(s"Trying to replace undefined parameter $varName; ignored")

          inVar = false
          varName = ""

        case _ if inVar  => varName += c
        case _ if !inVar => output += c
      }

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
        for (m <- config.getClass.getMethods if m.getReturnType == "".getClass && config.getClass.getMethods.exists(_.getName == m.getName + "_$eq")) {
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
        for (m <- config.getClass.getMethods if m.getReturnType == ListBuffer("").getClass && config.getClass.getMethods.exists(_.getName == m.getName + "_$eq")) {
          // get name and value of current parameter
          val paramName = m.getName
          val toProcess = m.invoke(config).asInstanceOf[ListBuffer[String]]

          val processed = toProcess.map(processString)

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