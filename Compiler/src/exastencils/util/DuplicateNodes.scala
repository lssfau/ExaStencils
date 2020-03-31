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

import exastencils.base.ir.IR_Node
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.ir.IR_StackCollector

object DuplicateNodes extends DefaultStrategy("Eliminate multiple usage of node instances") {
  // note: instances must be cleared between runs
  var instances = new java.util.IdentityHashMap[Node, List[IR_Node]]()

  var collector = new IR_StackCollector()
  this.register(collector)
  this.onBefore = () => {
    instances.clear()
    this.resetCollectors()
  }

  var printWarnings = true
  var printStack = false

  this += new Transformation("Duplicate", new PartialFunction[Node, Transformation.OutputType] {
    override def isDefinedAt(node : Node) : Boolean = {
      if (instances.containsKey(node))
        return true
      instances.put(node, collector.stack)
      false
    }

    override def apply(node : Node) : Transformation.OutputType = {
      val dup = Duplicate(node)
      if ((node ne dup) && printWarnings) {
        Logger.warn("Eliminated double reference by cloning: " + node)
        if (printStack) {
          val location1 = collector.stack
          val location2 = instances.get(node)
          Logger.warn("  location 1 parents are: " + location1.view.map(n => n.getClass.getName).mkString(" => "))
          Logger.warn("  location 2 parents are: " + location2.view.map(n => n.getClass.getName).mkString(" => "))
        }
      }
      dup
    }
  }
  )
}
