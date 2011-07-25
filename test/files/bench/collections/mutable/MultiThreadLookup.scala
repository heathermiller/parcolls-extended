import Global._
import scala.testing.Benchmark
import scala.collection.mutable.ConcurrentTrie

/* NOTE: Commented out benchmark which used prototype ctrie implementation 
 * in package ctrie (using only benchmarks which used prototype 
 * implementation in package ctrie2)
 * 
 * To compile,
 * Cliff's HM is located in test/files/lib/high-scale-lib.jar.
 * 
 * running the benchmark, example usage:
 * scala -Dsz=1000000 -Dpar=4 -cp classes/ MultiLookupCHM 10
 *
 * running the benchmark on Cliff's HM:
 * scala -Dsz=1000000 -Dpar=4 -cp classes/:../../../lib/high-scale-lib.jar MultiLookupCliff 10
 *  
 */

object MultiLookupCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  val chm = new ConcurrentHashMap[Elem, Elem]
  for (i <- 0 until sz) chm.put(elems(i), elems(i))
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Looker(chm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Looker(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        chm.get(e(i))
        i += 1
      }
    }
  }
}


object MultiLookupSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  val skiplist = new ConcurrentSkipListMap[Elem, Elem]
  for (i <- 0 until sz) skiplist.put(elems(i), elems(i))
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Looker(skiplist, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Looker(skiplist: ConcurrentSkipListMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        skiplist.get(e(i))
        i += 1
      }
    }
  }
}

/*
object MultiLookupCtrie extends Benchmark {
  val ct = new ConcurrentTrie[Elem, Elem]
  for (i <- 0 until sz) ct.insert(elems(i), elems(i))
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Looker(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Looker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.lookup(e(i))
        i += 1
      }
    }
  }
}
*/

object MultiLookupCtrie2 extends Benchmark {
  val ct = new ConcurrentTrie[Elem, Elem]
  for (i <- 0 until sz) ct.update(elems(i), elems(i))
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Looker(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Looker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.lookup(e(i))
        i += 1
      }
    }
  }
}


object MultiLookupCliff extends Benchmark {
  import org.cliffc.high_scale_lib._  
  
  val hm = new NonBlockingHashMap[Elem, Elem]
  for (i <- 0 until sz) hm.put(elems(i), elems(i))
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Looker(hm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Looker(hm: NonBlockingHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        hm.get(e(i))
        i += 1
      }
    }
  }
}

