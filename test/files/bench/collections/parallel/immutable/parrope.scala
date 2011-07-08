/** Test the Scala implementation of trait
 *  <code>scala.collection.parallel.immutable.ParRope</code>.
 *  
 *  @author Heather Miller
 */
import scala.collection.parallel.immutable.ParRope
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray
import scala.collection.immutable.Rope
import scala.testing.Benchmark


/*
 * FOREACH
 */
object foreachParArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  val parArray = txtArray.par
  var i: Int = 0

  def run = {
    parArray.foreach(elem => i += 1 )
  }

}

object foreachParRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  val parRope = new ParRope(txtRope)
  var i: Int = 0

  def run = {
    parRope.foreach(elem => i += 1 )
  }
}

object foreachParVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  val parVector = new ParVector(txtVector)
  var i: Int = 0

  def run = {
    parVector.foreach(elem => i += 1 )
  }
}

/*
 * FOLDLEFT
 */
object foldLeftParRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  val parRope = new ParRope(txtRope)
  var i: Int = 0

  def run = {
    i= parRope.foldLeft(0)((x, y) => x + 1)
  }
}

object foldLeftParVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  val parVector = new ParVector(txtVector)
  var i: Int = 0

  def run = {
    i = parVector.foldLeft(0)((x, y) => x + 1)
  }
}

object foldLeftParArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  val parArray = txtArray.par
  var i: Int = 0

  def run = {
    i= parArray.foldLeft(0)((x, y) => x + 1)
  }
}

object mapParArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  val parArray = txtArray.par
  var i: Int = 0

  def run = {
    parArray.map(elem => i += 1 )
  }

}

object mapParRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  val parRope = new ParRope(txtRope)
  var i: Int = 0

  def run = {
    parRope.map(elem => i += 1 )
  }
}

object mapParVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  val parVector = new ParVector(txtVector)
  var i: Int = 0

  def run = {
    parVector.map(elem => i += 1 )
  }
}

/*
object flatMapArray extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object flatMapRope extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object flatMapVector extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}
*/

object filterParArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  val parArray = txtArray.par

  def run = {
    parArray.filter(_ == 't')
  }
}

object filterParRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  val parRope = new ParRope(txtRope)

  def run = {
    parRope.filter(_ == 't')
  }
}

object filterParVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  val parVector = new ParVector(txtVector)

  def run = {
    parVector.filter(_ == 't')
  }
}

object aggregateParArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  val parArray = txtArray.par

  def run = {
    parArray.aggregate(0)((x, y) => x + 1, _ + _)
  }
}

object aggregateParRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  val parRope = new ParRope(txtRope)

  def run = {
    parRope.aggregate(0)((x, y) => x + 1, _ + _)
  }
}

object aggregateParVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  val parVector = new ParVector(txtVector)

  def run = {
    parVector.aggregate(0)((x, y) => x + 1, _ + _)
  }
}
