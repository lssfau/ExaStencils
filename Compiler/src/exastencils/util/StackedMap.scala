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

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

object StackedMap {
  def apply[K, V]() = new StackedMap[K, V]()
}

class StackedMap[K, V] {

  private var stack : List[Map[K, V]] = Nil

  def push() : Unit = {
    stack = HashMap[K, V]() :: stack
  }

  def pop() : Unit = {
    stack = stack.tail
  }

  def contains(key : K) : Boolean = {
    for (map <- stack)
      if (map.contains(key))
        return true
    return false
  }

  def get(key : K) : Option[V] = {
    for (map <- stack)
      for (v <- map.get(key)) // Option
        return Some(v)
    return None
  }

  def put(key : K, value : V) : Unit = {
    stack.head.put(key, value)
  }

  def isEmpty() : Boolean = {
    for (map <- stack)
      if (!map.isEmpty)
        return false
    return true
  }

  def topIsEmpty() : Boolean = {
    stack.head.isEmpty
  }

  def clear() : Unit = {
    stack = Nil
  }
}
