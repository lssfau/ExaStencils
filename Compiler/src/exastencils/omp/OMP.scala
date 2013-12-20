package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.core._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

trait OMP_PotentiallyCritical {
  def addOMPDirective : OMP_Critical;
}

case class OMP_Critical(var body : ListBuffer[Statement]) extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  def this(body : Statement) = this(ListBuffer(body));

  def cpp : String = {
    (s"#pragma omp critical\n{\n"
      + body.map(stat => stat.cpp).mkString("\n")
      + s"\n}");
  }
};