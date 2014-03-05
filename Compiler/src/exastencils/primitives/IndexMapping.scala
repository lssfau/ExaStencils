package exastencils.primitives

import exastencils.knowledge._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

  var indexInner = new IndexRange();
  var indexOuter = new IndexRange();
  var indexBorder = new IndexRange();

  var indexOpposingInner = new IndexRange();
  var indexOpposingOuter = new IndexRange();
  var indexOpposingBorder = new IndexRange();

  def setIndices(field : Field, level : Int) {
    indexInner = fieldToIndexInner(dir, level);
    indexOuter = fieldToIndexOuter(dir, level);
    indexBorder = fieldToIndexBorder(dir, level);
    indexOpposingInner = fieldToIndexInner(dir.map(i => -i), level);
    indexOpposingOuter = fieldToIndexOuter(dir.map(i => -i), level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), level);
  }

  def setIndicesWide(field : Field, level : Int) {
    indexInner = fieldToIndexInnerWide(dir, level);
    indexOuter = fieldToIndexOuterWide(dir, level);
    indexBorder = fieldToIndexBorder(dir, level);
    indexOpposingInner = fieldToIndexInnerWide(dir.map(i => -i), level);
    indexOpposingOuter = fieldToIndexOuterWide(dir.map(i => -i), level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), level);
  }
}

case class IndexRange(begin : Array[Int] = Array(0, 0, 0), end : Array[Int] = Array(0, 0, 0)) {}

case class IndexRange_Steffan(begin : Array[_ <: Any] = Array(0, 0, 0), end : Array[_ <: Any] = Array(0, 0, 0)) {} // FIXME: use regular IndexRange

object Mapping {
  def first(level : Int, dim : Int) : Int = {
    return 0;
  }
  def last(level : Int, dim : Int) : Int = {
    return numPoints(level, dim) - 1;
  }
  def numPoints(level : Int, dim : Int) : Int = {
    if (dim < Knowledge.dimensionality)
      return (Knowledge.fragLengthPerDim(dim) * (1 << level)) + 1 + 2 * Knowledge.numGhostLayers;
    else
      return 1;
  }
  def access(level : Int, x : String = "x", y : String = "y", z : String = "z") : String = {
    return (s"$x" +
      (if (Knowledge.dimensionality > 1) s" + $y * ${numPoints(level, 0)}" else "") +
      (if (Knowledge.dimensionality > 2) s" + $z * ${numPoints(level, 1) * numPoints(level, 0)}" else ""));
  }
  def access(level : Int, index : Array[Int]) : String = {
    return s"${index(2) * numPoints(level, 1) * numPoints(level, 0) + index(1) * numPoints(level, 0) + index(0)}";
  }
}

object fieldToIndexInner extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers + 1)
        case i if dir(i) > 0  => (Mapping.last(level, i) - 2 * Knowledge.numGhostLayers)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0),
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + 2 * Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers - 1)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0));
  }
}

object fieldToIndexInnerWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers + 1)
        case i if dir(i) > 0  => (Mapping.last(level, i) - 2 * Knowledge.numGhostLayers)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0),
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + 2 * Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers - 1)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0));
  }
}

object fieldToIndexOuter extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i))
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers + 1)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0),
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers - 1)
        case i if dir(i) > 0  => (Mapping.last(level, i))
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0));
  }
}

object fieldToIndexOuterWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i))
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers + 1)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0),
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i))
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers - 1)
        case i if dir(i) > 0  => (Mapping.last(level, i))
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0));
  }
}

object fieldToIndexBorder extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0),
      (0 until Knowledge.dimensionality).toArray.map(i => i match {
        case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.numGhostLayers)
        case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.numGhostLayers)
        case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.numGhostLayers)
      }) ++
        (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0));
  }
}
