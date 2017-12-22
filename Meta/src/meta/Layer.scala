package meta

import scala.collection.mutable.ListBuffer

/// Layer

object Layer {
  def all = ListBuffer(L1, L2, L3, L4, IR)
  def allButIR = ListBuffer(L1, L2, L3, L4)
  def L2AndUp = ListBuffer(L2, L3, L4, IR)
  def L3AndUp = ListBuffer(L3, L4, IR)
  def L4AndUp = ListBuffer[Layer](L4, IR)
  def L1_L2_L3 = ListBuffer(L1, L2, L3)
  def L2_L3 = ListBuffer(L2, L3)
  def L3_L4 = ListBuffer(L3, L4)
  def L2_L3_L4 = ListBuffer(L2, L3, L4)

  def matchLc(lc : String) = lc match {
    case "l1"   => L1
    case "l2"   => L2
    case "l3"   => L3
    case "l4"   => L4
    case "ir"   => IR
    case "meta" => MetaLayer
  }

  def matchLayerList(l : ListBuffer[Layer]) = l match {
    case ListBuffer(L1) => "L1"
    case ListBuffer(L2) => "L2"
    case ListBuffer(L3) => "L3"
    case ListBuffer(L4) => "L4"
    case ListBuffer(IR) => "IR"

    case ListBuffer(IR, L1, L2, L3, L4) => "all"
    case ListBuffer(L1, L2, L3, L4)     => "allButIR"
    case ListBuffer(IR, L2, L3, L4)     => "L2AndUp"
    case ListBuffer(IR, L3, L4)         => "L3AndUp"
    case ListBuffer(IR, L4)             => "L4AndUp"
    case ListBuffer(L1, L2, L3)         => "L1_L2_L3"
    case ListBuffer(L2, L3)             => "L2_L3"
    case ListBuffer(L3, L4)             => "L3_L4"
    case ListBuffer(L2, L3, L4)         => "L2_L3_L4"

    case other if other.contains(IR) => s"""ListBuffer(${ (other.last +: other.dropRight(1)).map(_.uc).mkString(", ") })"""

    case other => s"""ListBuffer(${ other.map(_.uc).mkString(", ") })"""
  }
}

abstract class Layer {
  def lc : String
  def uc : String
  def hasNext : Boolean
  def next : Layer
}

/// L1

object L1 extends Layer {
  override def lc = "l1"
  override def uc = "L1"
  override def hasNext = true
  override def next = L2
}

/// L2

object L2 extends Layer {
  override def lc = "l2"
  override def uc = "L2"
  override def hasNext = true
  override def next = L3
}

/// L3

object L3 extends Layer {
  override def lc = "l3"
  override def uc = "L3"
  override def hasNext = true
  override def next = L4
}

/// L4

object L4 extends Layer {
  override def lc = "l4"
  override def uc = "L4"
  override def hasNext = true
  override def next = IR
}

/// IR

object IR extends Layer {
  override def lc = "ir"
  override def uc = "IR"
  override def hasNext = false
  override def next = { println("WARN: Using next of IR"); IR }
}

/// MetaLayer

object MetaLayer extends Layer {
  override def lc = "meta"
  override def uc = "ME"
  override def hasNext = false
  override def next = ???
}