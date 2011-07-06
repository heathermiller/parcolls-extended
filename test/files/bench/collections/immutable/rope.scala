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

/*
object reduceArray extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object reduceRope extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object reduceVector extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object mapArray extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object mapRope extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object mapVector extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

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

object filterArray extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object filterRope extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object filterVector extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object aggregateArray extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object aggregateRope extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}

object aggregateVector extends Benchmark {

val txt = "A short text..." * 500000
def run = {}

}
*/




