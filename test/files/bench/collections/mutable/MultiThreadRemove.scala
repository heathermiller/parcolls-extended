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
 * scala -Dsz=1000000 -Dpar=4 -cp classes/ MultiRemoveCHM 10
 *
 * running the benchmark on Cliff's HM:
 * scala -Dsz=1000000 -Dpar=4 -cp classes/:../../../lib/high-scale-lib.jar MultiRemoveCliff 10
 *  
 */

// uncomment setUp
object MultiRemoveCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  var chm = new ConcurrentHashMap[Elem, Elem]
  
  override def setUp() {
    chm = new ConcurrentHashMap[Elem, Elem]
    for (i <- 0 until sz) chm.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(chm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  var last: AnyRef = null
  override def tearDown {
    last = chm
  }
  
  class Remover(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        chm.remove(e(i))
        i += 1
      }
    }
  }
}


object MultiRemoveSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  var skiplist = new ConcurrentSkipListMap[Elem, Elem]
  
  override def setUp() {
    skiplist = new ConcurrentSkipListMap[Elem, Elem]
    for (i <- 0 until sz) skiplist.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(skiplist, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(skiplist: ConcurrentSkipListMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        skiplist.remove(e(i))
        i += 1
      }
    }
  }
}

/*
object MultiRemoveCtrie extends Benchmark {
  var ct = new ConcurrentTrie[Elem, Elem]
  
  override def setUp() {
    ct = new ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.insert(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.remove(e(i))
        i += 1
      }
    }
  }
}
*/

object MultiRemoveCtrie2 extends Benchmark {
  var ct = new ConcurrentTrie[Elem, Elem]
  
  override def setUp() {
    ct = new ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.update(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.remove(e(i))
        i += 1
      }
    }
  }
}


object MultiRemoveCliff extends Benchmark {
  import org.cliffc.high_scale_lib._  
  
  var hm = new NonBlockingHashMap[Elem, Elem]
  
  override def setUp() {
    hm = new NonBlockingHashMap[Elem, Elem]
    for (i <- 0 until sz) hm.put(elems(i), elems(i))
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Remover(hm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Remover(hm: NonBlockingHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        hm.remove(e(i))
        i += 1
      }
    }
  }
}


