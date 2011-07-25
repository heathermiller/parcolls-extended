import Global._
import scala.testing.Benchmark
import scala.collection.mutable.ConcurrentTrie

/* NOTE: Commented out benchmark which used prototype ctrie implementation 
 * in package ctrie (using only benchmarks which used prototype 
 * implementation in package ctrie2)
 * 
 * running the benchmark, example usage:
 * scala -Dsz=1000000 -Dpar=4 -Dlookupratio=4 -cp classes/ LookupInsertsCHM 10 
 */

object LookupInsertsCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  def run() {
    val chm = new ConcurrentHashMap[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Worker(chm, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Worker(chm: ConcurrentHashMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      val ratio = lookupratio.get
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        chm.put(e(i), e(i))
        i += 1
        
        // do some lookups
        var j = 0
        while (j < ratio) {
          chm.get(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}


object LookupInsertsSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  def run() {
    val skiplist = new ConcurrentSkipListMap[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Worker(skiplist, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Worker(skiplist: ConcurrentSkipListMap[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      val ratio = lookupratio.get
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        skiplist.put(e(i), e(i))
        i += 1
        
        // do some lookups
        var j = 0
        while (j < ratio) {
          skiplist.get(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}


/*object LookupInsertsCtrie extends Benchmark {
  def run() {
    val ct = new ConcurrentTrie[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Worker(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      val ratio = lookupratio.get
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an insert
        ct.insert(e(i), e(i))
        i += 1
        
        // do some lookups
        var j = 0
        while (j < ratio) {
          ct.lookup(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}
*/

object LookupUpdatesCtrie2 extends Benchmark {
  def run() {
    val ct = new ConcurrentTrie[Elem, Elem]
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Worker(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Worker(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
    override def run() {
      val ratio = lookupratio.get
      var i = n * step
      val until = (n + 1) * step
      val e = elems
      while (i < until) {
        // do an update
        ct.update(e(i), e(i))
        i += 1
        
        // do some lookups
        var j = 0
        while (j < ratio) {
          ct.lookup(e(math.abs(j * 0x9e3775cd) % i))
          j += 1
        }
      }
    }
  }
}


