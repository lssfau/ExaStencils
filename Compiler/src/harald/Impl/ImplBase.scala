package harald.Impl

 class ImplBase() {
    var CostInfo: collection.mutable.Map[String, Map[Int,String]] = collection.mutable.Map()
    override def toString = ""
    def toString_cpp: String = ""
    def toString_cuda: String = ""
    def costs(para: String) : Map[Int,String] = Map()
    def calls(lev: Int) {}
 }
