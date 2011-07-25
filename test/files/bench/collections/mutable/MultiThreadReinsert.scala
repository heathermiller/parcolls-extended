import Global._
import scala.testing.Benchmark
import scala.collection.mutable.ConcurrentTrie

/* NOTE: Commented out benchmark which used prototype ctrie implementation 
 * in package ctrie (using only benchmarks which used prototype 
 * implementation in package ctrie2)
 * 
 * running the benchmark, example usage:
 * scala -Dsz=1000000 -Dpar=4 -cp classes/ MultiReinsertCHM 10
 */

object MultiReinsertCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  var chm = new ConcurrentHashMap[Elem, Elem]
  
  override def setUp {
    chm = new ConcurrentHashMap[Elem, Elem]
    for (i <- 0 until sz) chm.put(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
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


object MultiReinsertSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  var skiplist = new ConcurrentSkipListMap[Elem, Elem]
  
  override def setUp {
    skiplist = new ConcurrentSkipListMap[Elem, Elem]
    for (i <- 0 until sz) skiplist.put(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
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
object MultiReinsertCtrie extends Benchmark {
  var ct = new ConcurrentTrie[Elem, Elem]
  
  override def setUp {
    ct = new ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.insert(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
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

object MultiReupdateCtrie2 extends Benchmark {
  var ct = new ConcurrentTrie[Elem, Elem]
  
  override def setUp {
    ct = new ConcurrentTrie[Elem, Elem]
    for (i <- 0 until sz) ct.update(elems(i), elems(i))
    Runtime.getRuntime.gc()
  }
  
  def run() {
    val p = par.get
    val step = sz / p
    
    val ins = for (i <- 0 until p) yield new Updateer(ct, i, step)
    
    for (i <- ins) i.start()
    for (i <- ins) i.join()
  }
  
  class Updateer(ct: ConcurrentTrie[Elem, Elem], n: Int, step: Int) extends Thread {
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


