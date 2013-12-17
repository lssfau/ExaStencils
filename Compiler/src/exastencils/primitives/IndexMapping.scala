package exastencils.primitives

import exastencils.knowledge._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (2 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

  var indexInner = new IndexRange();
  var indexOuter = new IndexRange();
  var indexBorder = new IndexRange();

  var indexOpposingInner = new IndexRange();
  var indexOpposingOuter = new IndexRange();
  var indexOpposingBorder = new IndexRange();

  def setIndices(field : Field, level : Int) {
    indexInner = fieldToIndexInner(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOuter = fieldToIndexOuter(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexBorder = fieldToIndexBorder(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingInner = fieldToIndexInner(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingOuter = fieldToIndexOuter(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  }

  def setIndicesWide(field : Field, level : Int) {
    indexInner = fieldToIndexInnerWide(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOuter = fieldToIndexOuterWide(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexBorder = fieldToIndexBorder(dir, s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingInner = fieldToIndexInnerWide(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingOuter = fieldToIndexOuterWide(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), s"fragments[e]->${field.codeName}[slot][$level]", level);
  }
}

case class IndexRange(begin : Array[String] = Array("0", "0", "0"), end : Array[String] = Array("0", "0", "0")) {}

object Mapping {
  def first(level : Int) : Int = {
    return 0;
  }
  def last(level : Int) : Int = {
    return numPoints(level) - 1;
  }
  def numPoints(level : Int) : Int = {
    return (1 << level) + 1 + 2 * Knowledge.numGhostLayers;
  }
  def access(level : Int, z : String = "z", y : String = "y", x : String = "x") : String = {
    return s"$z * (${numPoints(level)} * ${numPoints(level)}) + $y * ${numPoints(level)} + $x";
  }
}

object fieldToIndexInner extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - Knowledge.numGhostLayers}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - 1}"
      }));
  }
}

object fieldToIndexInnerWide extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - Knowledge.numGhostLayers}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers - 1}"
      }));
  }
}

object fieldToIndexOuter extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level)}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers + 1}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers - 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level)}"
      }));
  }
}

object fieldToIndexOuterWide extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level)}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers + 1}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level)}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers - 1}"
        case i if dir(i) > 0  => s"${Mapping.last(level)}"
      }));
  }
}

object fieldToIndexBorder extends ((Array[Int], String, Int) => IndexRange) {
  def apply(dir : Array[Int], field : String, level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
        case i if dir(i) < 0  => s"${Mapping.first(level) + Knowledge.numGhostLayers}"
        case i if dir(i) > 0  => s"${Mapping.last(level) - Knowledge.numGhostLayers}"
      }));
  }
}
