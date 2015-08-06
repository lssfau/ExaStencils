package exastencils.polyhedron

import isl.Conversions._
import java.io.FileOutputStream
import java.io.InputStream

object Isl {

  private var loaded : Boolean = false
  def load() : Unit = {

    if (loaded) return

    val system : String =
      System.getProperty("os.name") match {
        case x if (x.startsWith("Windows")) => "win32"
        case x if (x.startsWith("Linux"))   => "linux"
        case x if (x.startsWith("Mac"))     => "darwin"
        case x if (x.startsWith("Darwin"))  => "darwin"
        case x =>
          throw new Exception("unknown operating system (" + x + "), cannot load native library isl")
      }
    val arch : String =
      System.getProperty("os.arch") match {
        case "amd64"     => "x86-64"
        case "x86_64"    => "x86-64"
        case "i386"      => "x86"
        case "powerpc64" => "ppc64"
        case x =>
          throw new Exception("unknown system architecture (" + x + "), cannot load native library isl")
      }

    val dir : String = if (system == "darwin") system else system + '-' + arch

    val Array(fname, fext) = System.mapLibraryName("isl").split('.')
    val is : InputStream = ClassLoader.getSystemResourceAsStream(dir + '/' + fname + '.' + fext)

    val tmpIslLib = java.io.File.createTempFile(fname, '.' + fext)
    val fos = new FileOutputStream(tmpIslLib)

    val buffer = new Array[Byte](64 * 2 ^ 10)
    try {
      Stream.continually(is.read(buffer))
        .takeWhile(_ > 0)
        .foreach { l => fos.write(buffer, 0, l) }
    } finally {
      fos.flush()
      fos.close()
      is.close()
    }

    tmpIslLib.deleteOnExit()

    System.load(tmpIslLib.getAbsolutePath())
    loaded = true
  }

  def initCtx() : isl.Ctx = {
    this.load()
    return isl.Ctx.alloc()
  }

  final lazy val ctx = initCtx()

  // names are intended to be imported (using "import exastencils.polyhedron.Isl.TypeAliases._")
  object TypeAliases {
    final val T_PAR = isl.DimType.Param
    final val T_SET = isl.DimType.Set
    final val T_IN = isl.DimType.In
    final val T_OUT = isl.DimType.Out
  }

  def simplify(uset : isl.UnionSet) : isl.UnionSet = {
    if (uset == null)
      return null
    var nju : isl.UnionSet = isl.UnionSet.empty(uset.getSpace())
    uset.coalesce().foreachSet({ set : isl.Set => nju = nju.addSet(set.removeRedundancies()) })
    return nju
  }

  def simplify(umap : isl.UnionMap) : isl.UnionMap = {
    if (umap == null)
      return null
    var nju : isl.UnionMap = isl.UnionMap.empty(umap.getSpace())
    umap.coalesce().foreachMap({ map : isl.Map => nju = nju.addMap(map.removeRedundancies()) })
    return nju
  }

  def simplify(set : isl.Set) : isl.Set = {
    if (set == null)
      return null
    return set.coalesce().removeRedundancies()
  }

  def simplify(map : isl.Map) : isl.Map = {
    if (map == null)
      return null
    return map.coalesce().removeRedundancies()
  }
}
