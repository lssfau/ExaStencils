package exastencils.datastructures.l1

import exastencils.datastructures._
import exastencils.datastructures.l1._
import exastencils.logger._

case class Mapping(var identifier : String, var set : MathSet) extends Definition

abstract class MathSet extends Node
case class MathSetR1() extends MathSet
case class MathSetR2() extends MathSet
case class MathSetR3() extends MathSet
case class MathSetC1() extends MathSet
case class MathSetC2() extends MathSet
case class MathSetC3() extends MathSet

object MathSet {
  def apply(id : String, exponent : String) : MathSet = {
    (id, exponent) match {
      case ("R", "1") => return MathSetR1()
      case ("R", "2") => return MathSetR2()
      case ("R", "3") => return MathSetR3()
      case ("C", "1") => return MathSetC1()
      case ("C", "2") => return MathSetC2()
      case ("C", "3") => return MathSetC3()
      case _          => Logger.error(s"Unknown math set ${id}${exponent}")
    }
  }

  def apply(id : String) : MathSet = {
    id match {
      case "R²" => return MathSetR2()
      case "R³" => return MathSetR3()
      case "C²" => return MathSetC2()
      case "C³" => return MathSetC3()
    }
  }
}
