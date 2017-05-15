package exastencils.base.l1

sealed abstract class L1_StencilParameter(val dim : Int, val len : Int) {
  def *(e : L1_StencilEntry) : L1_StencilParameter
  def /(e : L1_StencilEntry) : L1_StencilParameter
}

object L1_StencilParameter {
  implicit def doubleToEntry(value : Double) : L1_StencilEntry = L1_StencilEntry(value)
  implicit def doubleSeqToEntrySeq(values : Double*) : Seq[L1_StencilEntry] = values map { x => L1_StencilEntry(x) }
}

final case class L1_StencilEntry(val value : Double) extends L1_StencilParameter(0, 1) {
  implicit def doubleToEntry(_value : Double) = L1_StencilEntry(_value)
  def *(stencil : L1_Stencil) : L1_Stencil = stencil * this
  def *(e : L1_StencilEntry) : L1_StencilEntry = value * e.value
  def /(e : L1_StencilEntry) : L1_StencilEntry = value / e.value
  def +(e : L1_StencilEntry) : L1_StencilEntry = value + e.value
  override def toString = value.toString
}

final class L1_Stencil(val entries : Seq[L1_StencilParameter]) extends L1_StencilParameter(1 + entries(0).dim, entries.length) {
  assert(validate)

  def +(other : L1_Stencil) : L1_Stencil = {
    assert(dim == other.dim)
    if (this.len < other.len) {
      (this expandTo other.len) + other
    } else if (this.len > other.len) {
      this + (other expandTo this.len)
    } else {
      val newEntries = if (dim == 1) {
        for (i <- 0 until this.len) yield entries(i).asInstanceOf[L1_StencilEntry] + other.entries(i).asInstanceOf[L1_StencilEntry]
      } else {
        for (i <- 0 until this.len) yield entries(i).asInstanceOf[L1_Stencil] + other.entries(i).asInstanceOf[L1_Stencil]
      }
      new L1_Stencil(newEntries)
    }
  }

  def -(other : L1_Stencil) : L1_Stencil = {
    this + (-1 * other)
  }

  def *(e : L1_StencilEntry) = new L1_Stencil(for (entry <- entries) yield entry * e)

  def /(e : L1_StencilEntry) = new L1_Stencil(for (entry <- entries) yield entry / e)

  def apply(i : Int) = {
    if (i >= len) {
      throw new IndexOutOfBoundsException()
    } else {
      entries(i)
    }
  }

  def buildEmptyEntry : L1_StencilParameter = {
    if (this.dim == 1) {
      L1_StencilEntry(0)
    } else {
      val subEntry = entries(0).asInstanceOf[L1_Stencil].buildEmptyEntry
      new L1_Stencil(Seq.fill(entries(0).len)(subEntry))
    }
  }

  def expandTo(length : Int) : L1_Stencil = {
    val lenDif = length - this.len
    assert(lenDif > 0)
    if (lenDif % 2 != 0) {
      throw new IllegalArgumentException("Cannot expand from odd to even (or even to odd) sized stencil")
    }

    def buildEmpty(dimensions : Int) : L1_StencilParameter = {
      if (dimensions == 1) L1_StencilEntry(0) else new L1_Stencil(Seq.fill(entries(0).len)(buildEmpty(dimensions - 1)))
    }
    val empty = buildEmpty(this.dim)
    val filler = Seq.fill(lenDif / 2)(empty)
    new L1_Stencil(filler ++ this.entries ++ filler)
  }

  def tensorProduct(other : L1_Stencil) : L1_Stencil = {
    assert(other.entries.forall { x => x.isInstanceOf[L1_StencilEntry] })
    val newEntries = for (oEntry <- other.entries.asInstanceOf[Seq[L1_StencilEntry]]) yield {
      new L1_Stencil(for (entry <- entries) yield entry * oEntry)
    }
    new L1_Stencil(newEntries)
  }

  override def toString = {
    dim match {
      case 1 => "[" + entries.map { _.toString }.reduceLeft { (x, y) => x + ", " + y } + "]\n"
      case 2 => entries.foldLeft("") { (x, y) => x + y.toString }
      //case 3 => (entries map { _.toString }) mkString "\n"
      case _ => {
        val stringList = for (i <- 0 until len) yield "[" + (i) + "]:\n" + entries(i).toString
        stringList.mkString("\n")
      }
    }
  }

  def validate : Boolean = {
    if (dim == 1)
      entries forall { x => x.isInstanceOf[L1_StencilEntry] }
    else {
      val dim = entries(0).dim
      val len = entries(0).len
      val bool1 = entries.forall { x => x.dim == dim && x.len == len && x.isInstanceOf[L1_Stencil] }
      val bool2 = entries.forall { _.asInstanceOf[L1_Stencil].validate }
      bool1 && bool2
    }
  }
}

/*
final class Stencil [T <: StencilParameter](val entries: Seq[T])(implicit t: TypeTag[T]) extends StencilParameter(1+entries(0).dim, entries.length) {
  //assert(Stencil validate this)
  assert( entries.forall { x => x.isInstanceOf[Entry] } // all entries must be of same SubType
          || entries.forall { x => x.isInstanceOf[Stencil[_]] } )
  assert( entries.forall { x => x.dim == entries(0).dim  } ) // all entries must be of same dimension
  assert( entries.forall { x => x.len == entries(0).len } ) // all entries must be of same length
  def *(other: Entry): StencilParameter = new Stencil( for (entry <- entries) yield entry*other )

  def tensorProduct(other: Stencil[Entry]): Stencil[Stencil[StencilParameter]] = {
    val newEntries = for (oEntry <- other.entries.asInstanceOf[Seq[Entry]]) yield {
      new Stencil (for (entry <- entries) yield entry*oEntry)
    }
    new Stencil(newEntries)
  }
}
*/

