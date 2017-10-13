import scala.collection.mutable.ListBuffer

import exastencils.app._
import exastencils.app.ir.IR_DefaultLayerHandler
import exastencils.base.ExaRootNode
import exastencils.config._
import exastencils.core._
import exastencils.core.logger.Logger_HTML
import exastencils.datastructures._
import exastencils.logger._
import exastencils.parsers.config._
import exastencils.prettyprinting._
import exastencils.util._

object Main {
  def localInitialize(args : Array[String]) = {
    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")

    StateManager.setRoot(ExaRootNode)

    // check from where to read input
    val settingsParser = new Settings_Parser()
    val knowledgeParser = new Knowledge_Parser()
    val platformParser = new Platform_Parser()
    if (args.length >= 1)
      settingsParser.parseFile(args(0))
    if (Settings.produceHtmlLog) Logger_HTML.init() // allows emitting errors and warning in knowledge and platform parsers
    if (args.length >= 2)
      knowledgeParser.parseFile(args(1))
    if (args.length >= 3)
      platformParser.parseFile(args(2))
    if (args.length >= 4)
      IR_DefaultLayerHandler.polyOptExplID = args(3).toInt

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
        sys.exit(0)
      }
    }

    // init buildfile generator, overrides settings file
    if ("MSVC" == Platform.targetCompiler)
      Settings.buildfileGenerators = ListBuffer("ProjectfileGenerator")

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming("Initializing")
  }

  def localShutdown() = {
    StateManager.root.nodes.clear()
    ExaRootNode.clear()

    if (Settings.timeStrategies) {
      StrategyTimer.print()
      StrategyTimer.clear()
    }

    if (Settings.produceHtmlLog)
      Logger_HTML.finish()
  }

  def print() = {
    Logger.dbg("Prettyprinting to folder " + new java.io.File(Settings.getOutputPath).getAbsolutePath)
    ExaLayerHandler.ir_handler.print()
    PrettyprintingManager.finish()
  }

  def main(args : Array[String]) : Unit = {
    try {
      // for runtime measurement
      val start : Long = System.nanoTime()

      localInitialize(args)
      ExaLayerHandler.initializeAllLayers()

      ExaLayerHandler.handleAllLayers()

      print()

      Logger.dbg("Done!")

      Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
      new CountNodes("number of printed nodes").apply()

      ExaLayerHandler.shutdownAllLayers()
      localShutdown()
    } catch {
      case e : Throwable =>
        Logger.warn(s"Critical error: ${ e.getMessage }")
        Logger.warn(s"Stack trace:\n${ e.getStackTrace.mkString("\n\tat ") }")

        if (Settings.produceHtmlLog)
          Logger_HTML.finish()

        throw e
    }
  }
}
