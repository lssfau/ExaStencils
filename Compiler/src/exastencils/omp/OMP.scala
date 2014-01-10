package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.core._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

trait OMP_PotentiallyCritical

case class OMP_Critical(var body : Any) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def cpp : String = {
    s"#pragma omp critical\n{\n" +
      (body match {
        case prettyPrintable : CppPrettyPrintable => prettyPrintable.cpp;
        case buf : ListBuffer[_] => buf.map(stat => stat match {	// TODO: Buffer support is currently not tested!
          case prettyPrintable : CppPrettyPrintable => prettyPrintable.cpp;
          case _                                    => "NON_PRINTABLE NODE ENCOUNTERED!";
        })
        case _ => "NON_PRINTABLE NODE ENCOUNTERED!";
      }) +
      s"\n}"
  }
};
