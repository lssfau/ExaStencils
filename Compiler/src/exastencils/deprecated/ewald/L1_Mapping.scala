package exastencils.deprecated.ewald

import exastencils.datastructures._
import exastencils.logger._

case class L1_Mapping(var identifier : String, var set : L1_MathSet) extends L1_Definition

abstract class L1_MathSet extends Node

case class L1_MathSetR1() extends L1_MathSet

case class L1_MathSetR2() extends L1_MathSet

case class L1_MathSetR3() extends L1_MathSet

case class L1_MathSetC1() extends L1_MathSet

case class L1_MathSetC2() extends L1_MathSet

case class L1_MathSetC3() extends L1_MathSet

object L1_MathSet {
  def apply(id : String, exponent : String) : L1_MathSet = {
    (id, exponent) match {
      case ("R", "1") => return L1_MathSetR1()
      case ("R", "2") => return L1_MathSetR2()
      case ("R", "3") => return L1_MathSetR3()
      case ("C", "1") => return L1_MathSetC1()
      case ("C", "2") => return L1_MathSetC2()
      case ("C", "3") => return L1_MathSetC3()
      case _          => Logger.error(s"Unknown math set ${ id }${ exponent }")
    }
  }

  def apply(id : String) : L1_MathSet = {
    id match {
      case "R²" => return L1_MathSetR2()
      case "R³" => return L1_MathSetR3()
      case "C²" => return L1_MathSetC2()
      case "C³" => return L1_MathSetC3()
    }
  }
}
