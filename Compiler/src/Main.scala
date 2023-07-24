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

import scala.collection.mutable.ListBuffer

import exastencils.app._
import exastencils.base.ExaRootNode
import exastencils.config._
import exastencils.core._
import exastencils.core.logger.Logger_HTML
import exastencils.datastructures._
import exastencils.logger._
import exastencils.parsers.config.Settings_Parser
import exastencils.polyhedron.IR_PolyOpt
import exastencils.prettyprinting._
import exastencils.util._

object Main {
  def localInitialize(args : Array[String]) = {
    //if (Settings.timeStrategies) -> right now this Schroedinger flag is neither true nor false
    StrategyTimer.startTiming("Initializing")

    StateManager.setRoot(ExaRootNode)

    // check from where to read input
    val settingsParser = new Settings_Parser(Settings)
    val knowledgeParser = new Settings_Parser(Knowledge)
    val platformParser = new Settings_Parser(Platform)
    if (args.length >= 1)
      settingsParser.parseFile(args(0))
    if (Settings.produceHtmlLog) Logger_HTML.init() // allows emitting errors and warning in knowledge and platform parsers
    if (args.length >= 2)
      knowledgeParser.parseFile(args(1))
    if (args.length >= 3)
      platformParser.parseFile(args(2))
    if (args.length >= 4)
      IR_PolyOpt.polyOptExplIDs = args(3)

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

    ObjectWithStateCollection.clearAll()
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

      ExaLayerHandler.scheduleAllLayers()

      ExaLayerHandler.handleAllLayers()

      print()

      Logger.dbg("Done!")

      Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e8) / 10.0 + " seconds")
      new CountNodes("number of printed nodes").apply()

      ExaLayerHandler.shutdownAllLayers()
      localShutdown()
    } catch {
      case e : LoggerError =>
        throw e
      case e : Throwable =>
        Logger.warn(s"Critical error: ${ e.getMessage }")
        Logger.warn(s"Stack trace:\n${ e.getStackTrace.mkString("\n\tat ") }")

        if (Settings.produceHtmlLog)
          Logger_HTML.finish()

        throw e
    }
  }
}
