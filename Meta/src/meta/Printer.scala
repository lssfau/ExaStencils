package meta

/// Printer

object Printer {
  def process(toProcess : String, layer : Layer) = {
    var string = toProcess

    if (string.contains("|LAYER_UC|"))
      string = string.replaceAllLiterally("|LAYER_UC|", layer.uc)
    if (string.contains("|LAYER_LC|"))
      string = string.replaceAllLiterally("|LAYER_LC|", layer.lc)

    if (string.contains("|NEXT_UC|"))
      string = string.replaceAllLiterally("|NEXT_UC|", layer.next.uc)
    if (string.contains("|NEXT_LC|"))
      string = string.replaceAllLiterally("|NEXT_LC|", layer.next.lc)

    string = string.replaceAll("\r\n", "\n")
    if (true)
      string = string.replaceAll("\n", "\r\n")

    string
  }
}

class Printer extends java.io.StringWriter {
  def <<(s : String) = write(s)
  def <<<(s : String) = write(s + "\n")
}
