/** Test the Scala implementation of trait
 *  <code>scala.collection.parallel.immutable.ParRope</code>.
 *  
 *  @author Heather Miller
 */
import scala.collection.parallel.immutable.ParRope
import scala.collection.immutable.Rope
import scala.testing.Benchmark

/* benchmarks to do:
 * goal: compare sequential against array or vector.
 * ops to test:
 * Compare ParRope against Rope, Array, ParArray, Vector.
 * Operations to benchmark:
 * foreach, reduce, map, flatMap, filter, aggregate
 */

object foreachArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  var i: Int = 0

  def run = {
    txtArray.foreach(elem => i += 1 )
  }

}

object foreachRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  var i: Int = 0

  def run = {
    txtRope.foreach(elem => i += 1 )
  }
}

object foreachVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  var i: Int = 0

  def run = {
    txtVector.foreach(elem => i += 1 )
  }
}

object foldLeftArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  var i: Int = 0

  def run = {
    i= txtArray.foldLeft(0)((x, y) => x + 1)
  }
}

object foldLeftRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  var i: Int = 0

  def run = {
    i= txtRope.foldLeft(0)((x, y) => x + 1)
  }
}

object foldLeftVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  var i: Int = 0

  def run = {
    i = txtVector.foldLeft(0)((x, y) => x + 1)
  }
}


object mapArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray
  var i: Int = 0

  def run = {
    txtArray.map(elem => i += 1 )
  }

}

object mapRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)
  var i: Int = 0

  def run = {
    txtRope.map(elem => i += 1 )
  }
}

object mapVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))
  var i: Int = 0

  def run = {
    txtVector.map(elem => i += 1 )
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

object filterArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray

  def run = {
    txtArray.filter(_ == 't')
  }
}

object filterRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)

  def run = {
    txtRope.filter(_ == 't')
  }
}

object filterVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))

  def run = {
    txtVector.filter(_ == 't')
  }
}

object aggregateArray extends Benchmark {

  val txt = "A short text..." * 500000
  val txtArray = txt.toArray

  def run = {
    txtArray.aggregate(0)((x, y) => x + 1, _ + _)
  }
}

object aggregateRope extends Benchmark {

  val txt = "A short text..." * 500000
  val txtRope = Rope(txt)

  def run = {
    txtRope.aggregate(0)((x, y) => x + 1, _ + _)
  }
}

object aggregateVector extends Benchmark {

  val txt = "A short text..." * 500000
  val txtVector = Vector.tabulate(txt.length)(idx => txt(idx))

  def run = {
    txtVector.aggregate(0)((x, y) => x + 1, _ + _)
  }
}

