package meta

/// Layer

abstract class Layer {
  def lc : String
  def uc : String
  def hasNext : Boolean
  def next : Layer
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
  override def next = ???
}

/// MetaLayer

object MetaLayer extends Layer {
  override def lc = "meta"
  override def uc = "ME"
  override def hasNext = false
  override def next = ???
}