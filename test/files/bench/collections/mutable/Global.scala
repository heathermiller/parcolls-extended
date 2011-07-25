
case class Elem(i: Int) extends Comparable[Elem] {
  def compareTo(y: Elem) = i - y.i

  override def hashCode = {
    // var hc = i * 0x9e3775cd
    // hc = java.lang.Integer.reverseBytes(hc)
    // hc * 0x9e3775cd
    
    var hc = i * 0x9e3775cd
    hc
    
    //var hc = i * 0x9e3775cd
    //hc + (hc << 16) + i
    
    //i
  }
}


object Global {
  val sz = System.getProperty("sz").toInt
  val par = Option(System.getProperty("par")).map(_.toInt)
  val lookups = Option(System.getProperty("lookups")).map(_.toInt)
  val inserts = Option(System.getProperty("inserts")).map(_.toInt)
  val removes = Option(System.getProperty("removes")).map(_.toInt)
  val totalops = Option(System.getProperty("totalops")).map(_.toInt)
  val lookupratio = Option(System.getProperty("lookupratio")).map(_.toInt)
  val damping = Option(System.getProperty("damping")).map(_.toDouble)
  val maxlinks = Option(System.getProperty("maxlinks")).map(_.toInt)
  val pagegenerator = Option(System.getProperty("pagegenerator"))
  val debug = Option(System.getProperty("debug")).map(_.toBoolean).getOrElse(false)
  val elems = (for (i <- 0 until (sz * 2)) yield Elem(i)).toArray
}


