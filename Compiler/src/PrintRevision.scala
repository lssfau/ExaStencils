import scala.io.Source

object PrintRevision {

  def main(args : Array[String]) : Unit = {
    val url = getClass.getResource("/revision.txt")
    val lines = Source.fromURL(url, "utf8").getLines().toList
    System.out.println(lines(0))
  }
}
