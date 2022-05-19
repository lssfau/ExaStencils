package exastencils.waLBerla.ir.util

object IR_WaLBerlaDirection extends Enumeration {

  // TODO: find more elegant solution
  def getDirnameFromArray(arr : Array[Int]) = arr match {
    case Array(0) | Array(0, 0) | Array(0, 0, 0)    => C
    case Array(-1) | Array(-1, 0) | Array(-1, 0, 0) => W
    case Array(1) | Array(1, 0) | Array(1, 0, 0)    => E
    case Array(0, -1) | Array(0, -1, 0)             => S
    case Array(0, 1) | Array(0, 1, 0)               => N
    case Array(0, 0, -1)                            => B
    case Array(0, 0, 1)                             => T

    case Array(-1, -1) | Array(-1, -1, 0) => SW
    case Array(-1, 1) | Array(-1, 1, 0)   => NW
    case Array(1, -1) | Array(1, -1, 0)   => SE
    case Array(1, 1) | Array(1, 1, 0)     => NE

    case Array(1, 0, -1)  => BE
    case Array(-1, 0, -1) => BW
    case Array(0, -1, -1) => BS
    case Array(0, 1, -1)  => BN
    case Array(1, 0, 1)   => TE
    case Array(-1, 0, 1)  => TW
    case Array(0, -1, 1)  => TS
    case Array(0, 1, 1)   => TN

    case Array(-1, -1, -1) => BSW
    case Array(-1, -1, 1)  => BSE
    case Array(-1, 1, -1)  => BNW
    case Array(-1, 1, 1)   => BNE
    case Array(1, -1, 1)   => TSE
    case Array(1, -1, -1)  => TSW
    case Array(1, 1, 1)    => TNE
    case Array(1, 1, -1)   => TNW
  }

  def isAxisDirection(dir : Int) = dir match {
    case N | S | W | E | B | T => true
    case _                     => false
  }

  private val C = 0 //!< Center
  private val N = 1 //!< North
  private val S = 2 //!< South
  private val W = 3 //!< West
  private val E = 4 //!< East
  private val T = 5 //!< Top
  private val B = 6 //!< Bottom
  private val NW = 7 //!< North-West
  private val NE = 8 //!< North-East
  private val SW = 9 //!< South-West
  private val SE = 10 //!< South-East
  private val TN = 11 //!< Top-North
  private val TS = 12 //!< Top-South
  private val TW = 13 //!< Top-West
  private val TE = 14 //!< Top-East
  private val BN = 15 //!< Bottom-North
  private val BS = 16 //!< Bottom-South
  private val BW = 17 //!< Bottom-West
  private val BE = 18 //!< Bottom-East
  private val TNE = 19 //!< Top-North-East
  private val TNW = 20 //!< Top-North-West
  private val TSE = 21 //!< Top-South-East
  private val TSW = 22 //!< Top-South-West
  private val BNE = 23 //!< Bottom-North-East
  private val BNW = 24 //!< Bottom-North-West
  private val BSE = 25 //!< Bottom-South-East
  private val BSW = 26 //!< Bottom-South-West
  private val INVALID_DIR = 27 //!< Invalid direction

  val directionFromString = Map(
    "C" -> C, "N" -> N, "S" -> S, "W" -> W, "E" -> E, "T" -> T, "B" -> B,
    "NW" -> NW, "NE" -> NE, "SW" -> SW, "SE" -> SE, "TN" -> TN, "TS" -> TS, "TW" -> TW, "TE" -> TE, "BN" -> BN, "BS" -> BS, "BW" -> BW, "BE" -> BE,
    "TNE" -> TNE, "TNW" -> TNW, "TSE" -> TSE, "TSW" -> TSW, "BNE" -> BNE, "BNW" -> BNW, "BSE" -> BSE, "BSW" -> BSW, "INVALID_DIR" -> INVALID_DIR)

  val stringFromDirection = directionFromString.map(_.swap)
}
