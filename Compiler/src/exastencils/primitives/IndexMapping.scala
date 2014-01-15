package exastencils.primitives

import exastencils.knowledge._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (2 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

  var indexInner = new IndexRange();
  var indexOuter = new IndexRange();
  var indexBorder = new IndexRange();
  var indexBorderWide = new IndexRange();

  var indexOpposingInner = new IndexRange();
  var indexOpposingOuter = new IndexRange();
  var indexOpposingBorder = new IndexRange();

  def setIndices(field : Field, level : Int) {
    indexInner = fieldToIndexInner(dir, level);
    indexOuter = fieldToIndexOuter(dir, level);
    indexBorder = fieldToIndexBorder(dir, level);
    indexBorderWide = fieldToIndexBorderWide(dir, level);
    indexOpposingInner = fieldToIndexInner(dir.map(i => -i), level);
    indexOpposingOuter = fieldToIndexOuter(dir.map(i => -i), level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), level);
  }

  def setIndicesWide(field : Field, level : Int) {
    indexInner = fieldToIndexInnerWide(dir, level);
    indexOuter = fieldToIndexOuterWide(dir, level);
    indexBorder = fieldToIndexBorder(dir, level);
    indexBorderWide = fieldToIndexBorderWide(dir, level);
    indexOpposingInner = fieldToIndexInnerWide(dir.map(i => -i), level);
    indexOpposingOuter = fieldToIndexOuterWide(dir.map(i => -i), level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), level);
  }
}

case class IndexRange(var begin : Array[Int] = Array(0, 0, 0), var end : Array[Int] = Array(0, 0, 0)) {}

object Mapping {
  def first(level : Int, dim : Int) : Int = {
    return 0;
  }
  def last(level : Int, dim : Int) : Int = {
    return numPoints(level, dim) - 1;
  }
  def numPoints(level : Int, dim : Int) : Int = {
    return (Knowledge.fragLengthPerDim(dim) * (1 << level)) + 1 + 2 * Knowledge.numGhostLayers;
  }
  def access(level : Int, x : String = "x", y : String = "y", z : String = "z") : String = {
    return s"$z * ${numPoints(level, 1) * numPoints(level, 0)} + $y * ${numPoints(level, 0)} + $x";
  }
  def access(level : Int, index : Array[Int]) : String = {
    return s"${index(2) * numPoints(level, 1) * numPoints(level, 0) + index(1) * numPoints(level, 0) + index(0)}";
  }
}

object fieldToIndexInner extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers + 1)
        case i if dir(i) > 0  => (Mapping.last(level, i) - 2 * Knowledge.numGhostLayers)
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + 2 * Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers - 1)
      }));
  }
}

object fieldToIndexInnerWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers + 1)
        case i if dir(i) > 0  => (Mapping.last(level, i) - 2 * Knowledge.numGhostLayers)
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + 2 * Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers - 1)
      }));
  }
}

object fieldToIndexOuter extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i))
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers + 1)
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers - 1)
        case i if dir(i) > 0  => (Mapping.last(level, i))
      }));
  }
}

object fieldToIndexOuterWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i))
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers + 1)
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers - 1)
        case i if dir(i) > 0  => (Mapping.last(level, i))
      }));
  }
}

object fieldToIndexBorder extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers)
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers)
      }));
  }
}
object fieldToIndexBorderWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers)
      }),
      (0 to 2).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers)
      }));
  }
}
