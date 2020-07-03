package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_StringConstant
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object IR_MatShape {
  def apply(shape : String) = {
    new IR_MatShape(shape)
  }
}

case class IR_MatShape(
    var shape : String,
    var infos : Option[ListBuffer[(String, Any)]] = None
) extends IR_Expression {
  override def datatype : IR_Datatype = ???
  override def prettyprint(out : PpStream) : Unit = ???

  // conditionally define size and substructure attributes with associative names
  def size(key : String) : Int = {
    if (infos.isDefined) {
      infos.get.find(s => s._1 == key).getOrElse(Logger.error("size not found"))._2 match {
        case s : String => s.toInt
        case _ => Logger.error("unexpected type, needs to be of type Int")
      }
    } else Logger.error("size not found")
  }
  def shape(key : String) : String = {
    if (infos.isDefined) {
      infos.get.find(s => s._1 == key).getOrElse(Logger.error("shape not found"))._2.asInstanceOf[String]
    } else Logger.error("shape not found")
  }
  def addInfo(name : String, value : Any) : IR_MatShape = {
    if (infos.isDefined) {
      infos.get.append((name, value))
    } else {
      infos = Some(ListBuffer[(String, Any)]((name, value)))
    }
    this
  }

  def toExprList() : ListBuffer[IR_Expression] = {
    var l = ListBuffer[IR_Expression](IR_StringConstant("shape=" + shape))
    if (infos.isDefined) l ++= infos.get.map(p => IR_StringConstant("--" + p._1 + "=" + p._2))
    l
  }
  def toStringList() : ListBuffer[String] = {
    var l = ListBuffer[String]("shape=" + shape)
    if (infos.isDefined) l ++= infos.get.map(p => p._1 + "=" + p._2)
    l

  }
}

