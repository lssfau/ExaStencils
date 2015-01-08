package exastencils.datastructures.l3

import exastencils.core.Logger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Environment(parent : Option[Environment] = None) {
  import Environment._

  val map = mutable.HashMap[String, Value]()

  /** This function returns a stored value. */
  def lookup(id : String) : Value = {
    if (map contains id) {
      map(id)
    } else {
      parent match {
        case Some(p) => p.lookup(id)
        case None    => Logger.error("Identifier " + id + " is undefined.")
      }
    }
  }

  /**
    * This function always returns an r-value.
    *  If id maps to an l-value it will be first dereferenced.
    */
  def lookupRValue(id : String) : RValue = {
    lookup(id) match {
      case v : LValue => v.deref
      case v : RValue => v
      case _          => throw new Exception("Environment corrupted. Found an entry which is neither an l- or an r-value.")
    }
  }

  /**
    * Always return an l-value.
    *  If id maps to an r-value this will raise an exception.
    */
  def lookupLValue(id : String) : LValue = {
    lookup(id) match {
      case v : LValue => v
      case _          => throw new Exception("Expected an l-value.")
    }
  }

  def bind(id : String, value : Value) : Unit = {

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

