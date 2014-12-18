package exastencils.datastructures.l3

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Environment {
  sealed class Item()
  case class VariableItem(val dcId : String, val scType : ScType) extends Item
  case class FunctionItem(val f : FunctionStatement) extends Item
  case class StaticValueItem(value : StaticValue) extends Item
}
class Environment(parent : Option[Environment] = None) {
  import Environment._

  val map = mutable.HashMap[String, Item]()

  def lookup(id : String) : Item = {
    if (map contains id) {
      map(id)
    } else {
      parent.get.lookup(id)
    }
  }

  def bind(id : String, value : Item) : Unit = {

    if (map contains id) {
      // do not allow rebinding
      throw new Exception("Symbol '" + id + "' already bound.")
    }

    map += id -> value
  }

  def bindNew(symbols : List[String], values : List[Item]) : Environment = {

    val env = new Environment()
    for ((s, v) <- symbols zip values) {
      env.map += s -> v
    }

    return env
  }

  override def toString() : String = {
    parent match {
      case Some(p) => map.mkString(", ") + "(" + p.toString + ")"
      case None    => map.mkString(", ")
    }
  }
}

