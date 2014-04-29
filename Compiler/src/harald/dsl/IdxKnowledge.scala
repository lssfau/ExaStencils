package harald.dsl

import scala.collection.mutable.ListBuffer

object IdxKnowledge extends ExaKnowledge {

  // map (i,j) -> idx 
  def mapidxToLinear(par: ListBuffer[String], mem: ListBuffer[String]): String = {
    par.length match {
      case 1 => return s"${par(0)}"
      case 2 => return s"${par(0)}*${mem(1)} + ${par(1)}"
      case 3 => return s"${par(0)}*${mem(1)}*${mem(2)} + ${par(1)}*${mem(2)} + ${par(2)}"
    }
  }

  def mapidxToLinearClamp(par: ListBuffer[String], mem: ListBuffer[String]): String = {
    DomainKnowledge.rule_dim() match {
      case 1 => return s"max(0, min(${par(0)}, ${mem(0)}-1))"
      case 2 => return s"max(0, min(${par(0)}, ${mem(0)}-1))*${mem(1)} + max(0, min(${par(1)}, ${mem(1)}-1))"
      case 3 => return s"max(0, min(${par(0)}, ${mem(0)}-1))*${mem(1)}*${mem(2)} + max(0, min(${par(1)}, ${mem(1)}-1))*${mem(2)} + max(0, min(${par(2)}, ${mem(2)}-1))"
    }
  }
  
  // for operator
  def StencilToidx(dim: Int, size: Int): ListBuffer[ListBuffer[Int]] = {
    dim match {
      case 1 => {
        size match {
          case 9 => {
            var l : ListBuffer[ListBuffer[Int]] = ListBuffer()
            for (i <- 0 to size-1)
              l += ListBuffer(i-4)
          }
        }
      }
      case 2 => {
        size match {
          case 1 => return ListBuffer(ListBuffer(0, 0))
          case 4 => return ListBuffer(ListBuffer(0, 0), ListBuffer(-1, 0), ListBuffer(0, -1), ListBuffer(-1, -1))
          case 5 => return ListBuffer(ListBuffer(0, 0), ListBuffer(1, 0), ListBuffer(-1, 0), ListBuffer(0, 1), ListBuffer(0, -1))
          case 9 => return ListBuffer(ListBuffer(0, 0), ListBuffer(1, 0), ListBuffer(-1, 0), ListBuffer(0, 1), ListBuffer(0, -1), ListBuffer(-1, -1), ListBuffer(-1, 1), ListBuffer(1, -1), ListBuffer(1, 1))
          case 81 => {
               var l : ListBuffer[ListBuffer[Int]] = ListBuffer()
                for (i <- 0 to 8)
                  for (j <- 0 to 8)
                    l += ListBuffer(i-4,j-4)
          }
        }
      }
      case 3 => {
        size match {
          case 1 => return ListBuffer(ListBuffer(0, 0, 0))
          case 7 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer(1, 0, 0), ListBuffer(0, 1, 0), ListBuffer(0, 0, 1), ListBuffer(-1, 0, 0), ListBuffer(0, -1, 0), ListBuffer(0, 0, -1))
          case 8 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer(-1, 0, 0), ListBuffer(0, -1, 0), ListBuffer(0, 0, -1),ListBuffer(-1, -1, 0), ListBuffer(-1, 0, -1), ListBuffer(0, -1, -1), ListBuffer(-1, -1, -1))
          case 27 => return ListBuffer(ListBuffer(0, 0, 0), ListBuffer(1, 0, 0), ListBuffer(0, 1, 0), ListBuffer(0, 0, 1), ListBuffer(-1, 0, 0), ListBuffer(0, -1, 0), ListBuffer(0, 0, -1),
            ListBuffer(1, 1, 0), ListBuffer(1, -1, 0), ListBuffer(-1, 1, 0), ListBuffer(-1, -1, 0),
            ListBuffer(0, 1, 1), ListBuffer(0, -1, 1), ListBuffer(0, 1, -1), ListBuffer(0, -1, -1),
            ListBuffer(1, 0, 1), ListBuffer(1, 0, -1), ListBuffer(-1, 0, 1), ListBuffer(-1, 0, -1),
            ListBuffer(-1, -1, -1), ListBuffer(1, -1, -1), ListBuffer(-1, 1, -1), ListBuffer(-1, -1, 1), ListBuffer(1, 1, 1), ListBuffer(-1, 1, 1), ListBuffer(1, -1, 1), ListBuffer(1, 1, -1))
          case 729 => {
               var l : ListBuffer[ListBuffer[Int]] = ListBuffer()
                for (i <- 0 to 8)
                  for (j <- 0 to 8)
                    for (k <- 0 to 8)
                      l += ListBuffer(i-4,j-4,k-4)
          }
        }
      }
    }
    return ListBuffer()
  }

  // for interpolation
  def IntStencilToidx(dim: Int, size: Int): ListBuffer[ListBuffer[Int]] = {
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
  
  def IdxToStencilEntry(dim: Int, size: Int, idx: ListBuffer[Int]): Int = {
    val stidxlist = StencilToidx(dim, size)
    var i = 0

    for (i <- 0 to stidxlist.length - 1)
      if (stidxlist(i) == idx)
        return i
    return -1
  }

  def mapidxTocoord(par: ListBuffer[String], lev: Int): ListBuffer[String] = {
    var sb: ListBuffer[String] = ListBuffer()
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

  def mapidxTocoordcast(par: ListBuffer[String], lev: Int): ListBuffer[String] = {
    var sb: ListBuffer[String] = ListBuffer()
    if (DomainKnowledge.domain_L1.get._2.equals("UnitSquare")) {
      for (i <- 0 to 1)
        sb += s"${par(i)}/${DomainKnowledge.arraysizes(lev)(i + 1)}.0"
    }
    if (DomainKnowledge.domain_L1.get._2.equals("UnitCube")) {
      for (i <- 0 to 2)
        sb += s"${par(i)}/${DomainKnowledge.arraysizes(lev)(i + 1)}.0"
    }
    return sb
  }

  def mapidxTocoordcast(par: ListBuffer[String], lev: String): ListBuffer[String] = {
    var sb: ListBuffer[String] = ListBuffer()
    if (DomainKnowledge.domain_L1.get._2.equals("UnitSquare")) {
      for (i <- 0 to 1)
        sb += s"${par(i)}*meshsize[${lev}]"
    }
    if (DomainKnowledge.domain_L1.get._2.equals("UnitCube")) {
      for (i <- 0 to 2)
        sb += s"${par(i)}*meshsize[${lev}]"
    }
    return sb
  }
  
  def mapidxTocoordDouble(par: ListBuffer[Int], lev: Int): ListBuffer[Double] = {
    var sb: ListBuffer[Double] = ListBuffer()
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

  def mapcoordToidxInt(par: ListBuffer[Double], lev: Int): ListBuffer[Int] = {
    var sb: ListBuffer[Int] = ListBuffer()
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

  def mapcoordToidxLoop(ulp: ListBuffer[Double], lrp: ListBuffer[Double], lev: Int): ListBuffer[Int] = {
    var idxulp = mapcoordToidxInt(ulp, lev)
    var idxlrp = mapcoordToidxInt(lrp, lev)

    var listdiff: ListBuffer[Int] = ListBuffer()
    for (i <- 0 to idxulp.length - 1)
      if (idxulp(i) == idxlrp(i))
        listdiff += 0
      else
        listdiff += (idxlrp(i) - idxulp(i)) / (idxulp(i) + idxlrp(i))

    return listdiff
  }

}