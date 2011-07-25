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
 * scala -Dsz=1000000 -Dpar=4 -cp classes/ MultiInsertCHM 10
 *
 * running the benchmark on Cliff's HM:
 * scala -Dsz=1000000 -Dpar=4 -cp classes/:../../../lib/high-scale-lib.jar MultiInsertCliff 10
 *  
 */

object MultiInsertCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  def run() {
    val chm = new ConcurrentHashMap[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(chm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        chm.put(e(i), e(i))
        i += 1
      }
    }
  }
}


object MultiInsertSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  def run() {
    val skiplist = new ConcurrentSkipListMap[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(skiplist, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Inserter(skiplist: ConcurrentSkipListMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        skiplist.put(e(i), e(i))
        i += 1
      }
    }
  }
}

/*
object MultiInsertCtrie extends Benchmark {
  def run() {
    val ct = new ConcurrentTrie[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Inserter(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }


  class Inserter(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.insert(e(i), e(i))
        i += 1
      }
    }
  }
}
*/


object MultiInsertCtrie2 extends Benchmark {
  def run() {
    val ct = new ConcurrentTrie[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Updater(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Updater(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        ct.update(e(i), e(i))
        i += 1
      }
    }
  }
}


object MultiInsertCliff extends Benchmark {
  import org.cliffc.high_scale_lib._
  
  def run() {
    val hm = new NonBlockingHashMap[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Updater(hm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Updater(hm: NonBlockingHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      
      while (i < until) {
        hm.put(e(i), e(i))
        i += 1
      }
    }
  }
}


