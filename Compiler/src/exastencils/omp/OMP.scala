package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.core._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

trait OMP_PotentiallyCritical

case class OMP_Critical(var body : Node) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    body match {
      case prettyPrintable : CppPrettyPrintable => s"#pragma omp critical\n{\n" + prettyPrintable.cpp + s"\n}";
      case _                                    => "NON_PRINTABLE NODE ENCOUNTERED!";
    }
  }
};