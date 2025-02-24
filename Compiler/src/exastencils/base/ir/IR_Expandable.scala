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

package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.prettyprinting.PpStream
import exastencils.scheduling.SingleSchedulable
import exastencils.util.ir.IR_LevelCollector

/// IR_Expandable

trait IR_Expandable {
  def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def expand() : Transformation.OutputType
}

/// IR_SpecialExpandable

trait IR_SpecialExpandable {
  def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

/// IR_Expand

object IR_Expand extends DefaultStrategy("Expand all applicable nodes") {
  val collector = new IR_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def doUntilDone(node : Option[Node] = None) = {
    do { apply(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    do { applyStandalone(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  this += new Transformation("Expand", {
    case expandable : IR_Expandable => expandable.expand()
  })
}

/// IR_ExpandInOnePass

// TODO: this strategy becomes somewhat obsolete as soon as trafos implement the required behavior directly
object IR_ExpandInOnePass extends DefaultStrategy("Expand all applicable nodes") {
  val collector = new IR_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Expand", {
    case expandable : IR_Expandable =>
      var nodes : ListBuffer[Node] = ListBuffer()
      nodes += expandable
      var expandedSth = false
      do {
        expandedSth = false
        for (n <- nodes.indices) {
          if (!expandedSth) {
            nodes(n) match {
              case expandable : IR_Expandable =>
                val output = expandable.expand()
                output.inner match {
                  case single : Node   => nodes.update(n, single)
                  case list : NodeList =>
                    val split = nodes.splitAt(n)
                    split._2.remove(0)
                    nodes = split._1 ++ list.nodes ++ split._2
                }
                expandedSth = true
              case _                          =>
            }
          }
        }
      } while (expandedSth)

      if (nodes.length == 1)
        nodes(0)
      else
        nodes
  })
}

/// IR_ExpandWrapper

object IR_ExpandWrapper extends SingleSchedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (Knowledge.useFasterExpand)
      IR_ExpandInOnePass.apply(applyAtNode)
    else
      IR_Expand.doUntilDone(applyAtNode)
  }
}
