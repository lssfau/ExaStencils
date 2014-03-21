package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

class AbstractStencil(val name : String, val datatype : String, val matlength1 : String, val matlength2 : String, val distype : String, val order : String, val location : String) {

  override def toString = "ExaOp: '" + name + "', type = '" + datatype + "', disc_type = '" + distype + "', order = '" + order + "'"

  def transform : Stencil = {

    var stencilbuf : ListBuffer[Stencil] = ListBuffer()
    var entries : Array[Double] = Array[Double]()

    var strop : ListBuffer[String] = ListBuffer()
    for (i <- 0 to DomainKnowledge.operator_L1.length - 1)
      strop += DomainKnowledge.operator_L1(i)._2

    val stkn = new DomainKnowledge.StencilKnowledge(DomainKnowledge.domain_L1.get._2, distype, order, strop)
    var weakform = ""
    var stlength = 1
    var storestencil = 0

    entries =
      stkn match {

        case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2", ListBuffer("Laplacian")) => Array[Double](4.0, -1.0, -1.0, -1.0, -1.0)
        case DomainKnowledge.StencilKnowledge("UnitSquare", "FE", "2", ListBuffer("Laplacian")) =>
          stlength = 9; storestencil = 1; DomainKnowledge.use_FE = true; weakform = "grad(v_())*grad(w_())"; Array[Double]()
        case DomainKnowledge.StencilKnowledge("UnitCube", "FD", "2", ListBuffer("Laplacian")) => Array[Double](6.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0)
        case DomainKnowledge.StencilKnowledge("UnitCube", "FE", "2", ListBuffer("Laplacian")) =>
          stlength = 27; storestencil = 1; weakform = "grad(v_())*grad(w_())"; Array[Double]()
        case _ => Array[Double]()
      }

    if (name.equals("RestrictionStencil")) {

      val stkn = new DomainKnowledge.StencilKnowledge(DomainKnowledge.domain_L1.get._2, distype, DomainKnowledge.restr_order_L3.getOrElse(2).toString, ListBuffer(""))

      entries =
        stkn match {

          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2", ListBuffer("")) => Array[Double](1.0, 0.5, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25)
          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "1", ListBuffer("")) => Array[Double](1.0)
          case DomainKnowledge.StencilKnowledge("UnitCube", "FD", "2", ListBuffer("")) => Array[Double](1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
            0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125)
        }
    }

    if (name.equals("CorrectionStencil")) {

      val stkn = new DomainKnowledge.StencilKnowledge(DomainKnowledge.domain_L1.get._2, distype, DomainKnowledge.restr_order_L3.getOrElse(2).toString, ListBuffer("")) // TODO

      entries =
        stkn match {

          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "2", ListBuffer("")) => Array[Double](0.25, 0.25, 0.25, 0.25)
          case DomainKnowledge.StencilKnowledge("UnitSquare", "FD", "1", ListBuffer("")) => Array[Double](1.0)
          case DomainKnowledge.StencilKnowledge("UnitCube", "FD", "2", ListBuffer(""))   => Array[Double](0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625, 0.0625)
        }
    }

    if (entries.length != 0)
      stlength = entries.length

    var xs = 1
    var ys = 1
    var zs = 1

    if (storestencil == 1) {
      xs = DomainKnowledge.xsize_L2.getOrElse(1)
      ys = DomainKnowledge.ysize_L2.getOrElse(1)
      zs = DomainKnowledge.zsize_L2.getOrElse(1)
    }

    if ((matlength1.toInt == 1) && (matlength2.toInt == 1)) {
      var stencil = new Stencil(name)
      for (i <- 0 until stlength)
        stencil.entries += StencilEntry(new MultiIndex(IdxKnowledge.StencilToidx(Knowledge.dimensionality, stlength)(i).toArray), entries(i))
      return stencil
    } else {
      println("FIXME: not implemented")
      new Stencil("FIXME")
    }
  }
}
