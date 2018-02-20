import scala.collection.mutable.ListBuffer

import exastencils.app.ExaLayerHandler
import exastencils.base.ExaRootNode
import exastencils.config._
import exastencils.core._
import exastencils.core.logger.Logger_HTML
import exastencils.datastructures.StrategyTimer
import exastencils.logger.Logger
import exastencils.parsers.config._
import exastencils.prettyprinting.PrettyprintingManager
import exastencils.runner._
import exastencils.util.CountNodes

object ConfigRunner {
  var configCollectionSnapshot = ListBuffer[(Object, Map[String, Any])]()
  var runnerConfig : RunnerConfig = _

  def initFirstTime(args : Array[String]) = {
    StateManager.setRoot(ExaRootNode)

    if (args.length >= 1) {
      val settingsParser = new Settings_Parser()
      settingsParser.parseFile(args(0))
    }
    if (Settings.produceHtmlLog) Logger_HTML.init() // allows emitting errors and warning in knowledge and platform parsers
    if (args.length >= 2) {
      val knowledgeParser = new Knowledge_Parser()
      knowledgeParser.parseFile(args(1))
    }
    if (args.length >= 3) {
      val platformParser = new Platform_Parser()
      platformParser.parseFile(args(2))
    }
    if (args.length >= 4) {
      val runnerParser = new Runner_Parser()
      if (RunnerConfig.debug)
        Logger.debug(s"Parsing samples file ${ args(3) }")
      runnerConfig = runnerParser.parseFile(args(3))
      if (RunnerConfig.debug)
        Logger.debug(runnerConfig.print())
    }

    // store config collections
    for (configCollection <- List(Knowledge, Settings, Platform)) {
      var valueMap = Map[String, Any]()

      // all members with setters
      for (m <- configCollection.getClass.getMethods if configCollection.getClass.getMethods.exists(_.getName == m.getName + "_$eq")) {
        valueMap += ((m.getName, m.invoke(configCollection)))

        configCollectionSnapshot += ((configCollection, valueMap))
      }
    }
  }

  def initSubsequent(configuration : Configuration) : Boolean = {
    if (Settings.timeStrategies)
      StrategyTimer.startTiming("Initializing")

    StateManager.setRoot(ExaRootNode)
    ExaRootNode.clear()

    // restore config collections
    for ((configCollection, valueMap) <- configCollectionSnapshot; v <- valueMap)
      UniversalSetter(configCollection, v._1, v._2)

    // update according to current configuration
    configuration.apply()
    runnerConfig.setDerivedParams()
    ResolveAlias.apply()

    // validate knowledge, etc.
    Knowledge.update()
    Settings.update()
    Platform.update()

    // resolve aliases in knowledge, settings and platform
    ResolveAlias.apply()

    // begin writing log to file after potential alias resolution in filename
    if (Settings.produceHtmlLog) Logger_HTML.beginFileWrite()

    if (Settings.cancelIfOutFolderExists) {
      if (new java.io.File(Settings.getOutputPath).exists()) {
        Logger.error(s"Output path ${ Settings.getOutputPath } already exists but cancelIfOutFolderExists is set to true. Shutting down now...")
        return false // throw this config away
      }
    }

    // init buildfile generator, overrides settings file
    if ("MSVC" == Platform.targetCompiler)
      Settings.buildfileGenerators = ListBuffer("ProjectfileGenerator")

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Initializing")

    true // go on
  }

  def main(args : Array[String]) : Unit = {
    // for runtime measurement
    val start : Long = System.nanoTime()

    //for (m <- config.getClass.getMethods if m.getReturnType == "".getClass && config.getClass.getMethods.exists(_.getName == m.getName + "_$eq")
    // todo: store initial config collections

    initFirstTime(args)

    val configNames = ListBuffer[String]()

    val configurations = runnerConfig.generateConfigurations()
    if (RunnerConfig.debug) {
      Logger.debug(s"Found ${ configurations.size } configurations:")
      configurations.foreach(config => Logger.debug(config.print()))
    }

    for (configuration <- configurations) {
      val localStart : Long = System.nanoTime()
      val goOn = initSubsequent(configuration)

      if (goOn && runnerConfig.constraints.map(_.eval()).fold(true)(_ && _)) {
        ExaLayerHandler.initializeAllLayers()
        ExaLayerHandler.handleAllLayers()

        Main.print()
        configNames += Settings.configName

        Logger.debug("Done!")

        Logger.debug("Runtime:\t" + math.round((System.nanoTime() - localStart) / 1e8) / 10.0 + " seconds")
        new CountNodes("number of printed nodes").apply()
      } else {
        if (RunnerConfig.debug)
          Logger.debug(s"Configuration filtered due to constraints: ${ configuration.print() }")
      }

      ExaLayerHandler.shutdownAllLayers()
      Main.localShutdown()
    }

    printCompileScript(configNames)
    printSubmitScript(configNames)

    Logger.debug(s"Generated ${ configNames.length } configurations:\n\t${ configNames.mkString(";\n\t") }")

    Logger.debug("Total runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
  }

  def printCompileScript(configNames : ListBuffer[String]) : Unit = {
    Platform.targetName.toLowerCase() match {
      case "i10staff40" =>
        val filename = "../compileGenerated"

        Logger.debug(s"Generating compile script for ${ Platform.targetName } with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        printer <<< "#!/bin/bash"
        printer <<< ""
        printer <<< "configurations=\"" + configNames.mkString(" ") + "\""
        printer <<< "for config in $configurations"
        printer <<< "do"
        printer <<< "\tcd ${config}"
        printer <<< "\ttime make -j 8"
        printer <<< "\tcd .."
        printer <<< "done"

        printer.finish()

      case "piz_daint" | "pizdaint" =>
        val filename = "../compileGenerated"

        Logger.debug(s"Generating compile script for ${ Platform.targetName } with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        printer <<< "#!/bin/bash"
        printer <<< ""
        printer <<< "configurations=\"" + configNames.mkString(" ") + "\""
        printer <<< "for config in $configurations"
        printer <<< "do"
        printer <<< "\tcd ${config}"
        printer <<< "\ttime make -j"
        printer <<< "\tcd .."
        printer <<< "done"

        printer.finish()

      case "tsubame" | "tsubame3" =>
        val filename = "../compileGenerated"

        Logger.debug(s"Generating compile script for ${ Platform.targetName } with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        printer <<< s"#!/bin/sh"

        // use current work directory to ensure correct location of output files
        printer <<< s"#$$-cwd"
        printer <<< s"#$$ -l f_node=${ Platform.hw_numNodes }"
        printer <<< s"#$$ -l h_rt=0:30:00"
        printer <<< s"#$$ -N compileGenerated"

        printer <<< ""

        if (Knowledge.cuda_enabled) {
          printer <<< s"## cuda path propagation"
          printer <<< s"#$$ -v LD_LIBRARY_PATH=/apps/t3/sles12sp2/cuda/8.0/lib64"
          printer <<< ""
        }

        // load modules
        printer <<< s". /etc/profile.d/modules.sh"

        // on this machine, mpi depends on cuda
        if (Knowledge.cuda_enabled || Knowledge.mpi_enabled)
          printer <<< s"module load cuda"

        if ("ICC" == Platform.targetCompiler)
          printer <<< s"module load intel"

        Platform.mpi_variant.toLowerCase() match {
          case "openmpi"  => printer <<< s"module load openmpi"
          case "intelmpi" => printer <<< s"module load intel-mpi"
          case other      => Logger.error(s"Unsupported mpi variant $other")
        }

        printer <<< ""
        printer <<< s"cd ~"

        printer <<< ""
        printer <<< "configurations=\"" + configNames.mkString(" ") + "\""
        printer <<< "for config in $configurations"
        printer <<< "do"
        printer <<< "\tcd ${config}"
        printer <<< "\ttime make -j"
        printer <<< "\tcd .."
        printer <<< "done"

        printer.finish()

      case other => Logger.warn(s"Found unknown target platform $other; could not set up compile script")
    }
  }

  def printSubmitScript(configNames : ListBuffer[String]) : Unit = {
    Platform.targetName.toLowerCase() match {
      case "i10staff40" =>
        val filename = "../runGenerated"

        Logger.debug(s"Generating compile script for ${ Platform.targetName } with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        printer <<< "#!/bin/bash"
        printer <<< ""
        printer <<< "configurations=\"" + configNames.mkString(" ") + "\""
        printer <<< "for config in $configurations"
        printer <<< "do"
        printer <<< "\tcd ${config}"
        printer <<< "\ttime ./exastencils > ${config}"
        printer <<< "\tcd .."
        printer <<< "done"

        printer.finish()

      case "piz_daint" | "pizdaint" =>
        val filename = "../submitGenerated"

        Logger.debug(s"Generating compile script for ${ Platform.targetName } with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        printer <<< "#!/bin/bash"
        printer <<< ""
        printer <<< "configurations=\"" + configNames.mkString(" ") + "\""
        printer <<< "for config in $configurations"
        printer <<< "do"
        printer <<< "\tcd ${config}"
        printer <<< "\tsbatch run"
        printer <<< "\tcd .."
        printer <<< "done"
        Settings.user.toLowerCase() match {
          case "sebastian" | "kuckuk" | "sebastiankuckuk" =>
            printer <<< "watch squeue -u kuckuk"
          case _                                          =>
          // nothing to do
        }

        printer.finish()

      case "tsubame" | "tsubame3" =>
        val filename = "../submitGenerated"

        Logger.debug(s"Generating compile script for ${ Platform.targetName } with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        printer <<< "#!/bin/bash"
        printer <<< ""
        printer <<< "configurations=\"" + configNames.mkString(" ") + "\""
        printer <<< "for config in $configurations"
        printer <<< "do"
        printer <<< "\tcd ${config}"
        printer <<< "\tqsub run"
        printer <<< "\tcd .."
        printer <<< "done"
        Settings.user.toLowerCase() match {
          case "sebastian" | "kuckuk" | "sebastiankuckuk" =>
            printer <<< "watch qstat"
          case _                                          =>
          // nothing to do
        }

        printer.finish()

      case other => Logger.warn(s"Found unknown target platform $other; could not set up submit script")
    }
  }
}