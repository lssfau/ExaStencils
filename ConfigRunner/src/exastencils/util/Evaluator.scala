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

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

// on error: go to file -> project structure -> modules -> config runner -> dependencies -> double click scala sdk -> standard library 'plus' -> .../scala-library/jars/scala-compiler-2.x.x.jar

import java.io.File

import exastencils.logger.Logger
import exastencils.runner.RunnerConfig

/// Evaluator

object Evaluator {
  // code inspired by https://gist.github.com/xuwei-k/9ba39fe22f120cb098f4

  def apply[A](expression : String) : A = {
    if (RunnerConfig.debug)
      Logger.debug("Evaluating " + expression)

    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(expression)

    val ret = toolbox.eval(tree).asInstanceOf[A]
    if (RunnerConfig.debug)
      Logger.debug("Evaluated to " + ret)
    ret
  }

  def fromFile[A](file : File) : A = {
    apply(scala.io.Source.fromFile(file).mkString(""))
  }

  def fromFile[A](file : String) : A = {
    fromFile(new File(file))
  }
}
