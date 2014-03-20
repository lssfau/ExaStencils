package harald.dsl

import scala.collection.mutable.ListBuffer
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

object IdxKnowledge {

  def mapidxToLinear(par : ListBuffer[String], mem : ListBuffer[String]) : String = {
    DomainKnowledge.rule_dim() match {
      case 1 => return s"${par(0)}"
      case 2 => return s"${par(0)}*${mem(1)} + ${par(1)}"
      case 3 => return s"${par(0)}*${mem(1)}*${mem(2)} + ${par(1)}*${mem(2)} + ${par(2)}"
    }
  }

  def mapidxToLinearClamp(par : ListBuffer[String], mem : ListBuffer[String]) : String = {
    DomainKnowledge.rule_dim() match {
      case 1 => return s"max(0, min(${par(0)}, ${mem(0)}-1))"
      case 2 => return s"max(0, min(${par(0)}, ${mem(0)}-1))*${mem(1)} + max(0, min(${par(1)}, ${mem(1)}-1))"
      case 3 => return s"max(0, min(${par(0)}, ${mem(0)}-1))*${mem(1)}*${mem(2)} + max(0, min(${par(1)}, ${mem(1)}-1))*${mem(2)} + max(0, min(${par(2)}, ${mem(2)}-1))"
    }
  }

  def StencilToidx(dim : Int, size : Int) : ListBuffer[ListBuffer[Expression]] = {
    dim match {
      case 2 => {
        size match {
          case 1 => return ListBuffer(ListBuffer(0, 0))
          case 4 => return ListBuffer(ListBuffer(0, 0), ListBuffer("x" Mod 2, 0), ListBuffer(0, "y" Mod 2), ListBuffer("x" Mod 2, "y" Mod 2))
          case 5 => return ListBuffer(ListBuffer(0, 0), ListBuffer(1, 0), ListBuffer(-1, 0), ListBuffer(0, 1), ListBuffer(0, -1))
          case 9 => return ListBuffer(ListBuffer(0, 0), ListBuffer(1, 0), ListBuffer(-1, 0), ListBuffer(0, 1), ListBuffer(0, -1), ListBuffer(-1, -1), ListBuffer(-1, 1), ListBuffer(1, -1), ListBuffer(1, 1))
        }
      }
      case 3 => {
        size match {
          case 1 => return ListBuffer(ListBuffer(0, 0, 0))
          case 7 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer(1, 0, 0), ListBuffer(0, 1, 0), ListBuffer(0, 0, 1), ListBuffer(-1, 0, 0), ListBuffer(0, -1, 0), ListBuffer(0, 0, -1))
          case 8 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer("x" Mod 2, 0, 0), ListBuffer(0, "y" Mod 2, 0), ListBuffer("x" Mod 2, "y" Mod 2, 0),
            ListBuffer(0, 0, "z" Mod 2), ListBuffer("x" Mod 2, 0, "z" Mod 2), ListBuffer(0, "y" Mod 2, "z" Mod 2), ListBuffer("x" Mod 2, "y" Mod 2, "z" Mod 2))
          case 27 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer(1, 0, 0), ListBuffer(0, 1, 0), ListBuffer(0, 0, 1), ListBuffer(-1, 0, 0), ListBuffer(0, -1, 0), ListBuffer(0, 0, -1),
            ListBuffer(1, 1, 0), ListBuffer(1, -1, 0), ListBuffer(-1, 1, 0), ListBuffer(-1, -1, 0),
            ListBuffer(0, 1, 1), ListBuffer(0, -1, 1), ListBuffer(0, 1, -1), ListBuffer(0, -1, -1),
            ListBuffer(1, 0, 1), ListBuffer(1, 0, -1), ListBuffer(-1, 0, 1), ListBuffer(-1, 0, -1),
            ListBuffer(-1, -1, -1), ListBuffer(1, -1, -1), ListBuffer(-1, 1, -1), ListBuffer(-1, -1, 1), ListBuffer(1, 1, 1), ListBuffer(-1, 1, 1), ListBuffer(1, -1, 1), ListBuffer(1, 1, -1))
        }
      }
    }
    return ListBuffer()
  }

  def IntStencilToidx(dim : Int, size : Int) : ListBuffer[ListBuffer[Int]] = {
    dim match {
      case 2 => {
        size match {
          case 4 => return ListBuffer(ListBuffer(0, 0), ListBuffer(1, 0), ListBuffer(0, 1), ListBuffer(1, 1))
        }
      }
      case 3 => {
        size match {
          case 8 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer(1, 0, 0), ListBuffer(0, 1, 0), ListBuffer(0, 0, 1), ListBuffer(1, 1, 0), ListBuffer(0, 1, 1), ListBuffer(1, 0, 1), ListBuffer(1, 1, 1))
        }
      }
    }
    return ListBuffer()
  }

  def IdxToStencilEntry(dim : Int, size : Int, idx : ListBuffer[Int]) : Int = {
    val stidxlist = StencilToidx(dim, size)
    var i = 0

    for (i <- 0 to stidxlist.length - 1)
      if (stidxlist(i) == idx)
        return i
    return -1
  }

  def mapidxTocoord(par : ListBuffer[String], lev : Int) : ListBuffer[String] = {
    var sb : ListBuffer[String] = ListBuffer()
    if (DomainKnowledge.domain_L1.get._2.equals("UnitSquare")) {
      for (i <- 0 to 1)
        sb += s"${par(i)}/${DomainKnowledge.arraysizes(lev)(i + 1)}"
    }
    if (DomainKnowledge.domain_L1.get._2.equals("UnitCube")) {
      for (i <- 0 to 2)
        sb += s"${par(i)}/${DomainKnowledge.arraysizes(lev)(i + 1)}"
    }
    return sb
  }

  def mapidxTocoordDouble(par : ListBuffer[Int], lev : Int) : ListBuffer[Double] = {
    var sb : ListBuffer[Double] = ListBuffer()
    if (DomainKnowledge.domain_L1.get._2.equals("UnitSquare")) {
      for (i <- 0 to 1)
        sb += par(i) / DomainKnowledge.arraysizes(lev)(i + 1).toDouble
    }
    if (DomainKnowledge.domain_L1.get._2.equals("UnitCube")) {
      for (i <- 0 to 2)
        sb += par(i) / DomainKnowledge.arraysizes(lev)(i + 1).toDouble
    }
    return sb
  }

  def mapcoordToidxInt(par : ListBuffer[Double], lev : Int) : ListBuffer[Int] = {
    var sb : ListBuffer[Int] = ListBuffer()
    if (DomainKnowledge.domain_L1.get._2.equals("UnitSquare")) {
      for (i <- 0 to 1)
        sb += (DomainKnowledge.arraysizes(lev)(i + 1) * par(i)).toInt
    }
    if (DomainKnowledge.domain_L1.get._2.equals("UnitCube")) {
      for (i <- 0 to 2)
        sb += (DomainKnowledge.arraysizes(lev)(i + 1) * par(i)).toInt
    }
    return sb
  }

  def mapcoordToidxLoop(ulp : ListBuffer[Double], lrp : ListBuffer[Double], lev : Int) : ListBuffer[Int] = {
    var idxulp = mapcoordToidxInt(ulp, lev)
    var idxlrp = mapcoordToidxInt(lrp, lev)

    var listdiff : ListBuffer[Int] = ListBuffer()
    for (i <- 0 to idxulp.length - 1)
      if (idxulp(i) == idxlrp(i))
        listdiff += 0
      else
        listdiff += (idxlrp(i) - idxulp(i)) / (idxulp(i) + idxlrp(i))

    return listdiff
  }

}