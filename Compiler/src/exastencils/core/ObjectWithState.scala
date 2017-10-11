package exastencils.core

import scala.collection.mutable.ListBuffer

/// ObjectWithState

trait ObjectWithState {
  ObjectWithStateCollection.add(this)

  def clear() = {}
  def init() = {}
}

/// ObjectWithStateCollection

object ObjectWithStateCollection {
  var objects = ListBuffer[ObjectWithState]()
  def add(obj : ObjectWithState) = objects += obj

  def clearAll() = objects.foreach(_.clear())
  def initAll() = objects.foreach(_.init())
}

