package exastencils.datastructures.l3

import scala.collection.mutable

import exastencils.logger._

class Environment(parent : Option[Environment] = None) {
  import Environment._

  val map = mutable.HashMap[String, StaticLocation]()

  /** This function returns a stored value. */
  def lookup(id : String) : StaticLocation = {
    if (map contains id) {
      map(id)
    } else {
      parent match {
        case Some(p) => p.lookup(id)
        case None    => Logger.error("Identifier " + id + " is undefined.")
      }
    }
  }

  def bind(id : String, value : StaticLocation) : Unit = {

    if (map contains id) {
      // do not allow rebinding
      throw new Exception("Symbol '" + id + "' already bound.")
    }

    map += id -> value
  }

  override def toString() : String = {
    parent match {
      case Some(p) => map.mkString(", ") + "(" + p.toString + ")"
      case None    => map.mkString(", ")
    }
  }

  def boundIdentifier() = map.keySet.toList
}

