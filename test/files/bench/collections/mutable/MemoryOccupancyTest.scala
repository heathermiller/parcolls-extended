import Global._
import scala.testing.Benchmark
import scala.collection.mutable.ConcurrentTrie

/* NOTE: Commented out benchmark which used prototype ctrie implementation 
 * in package ctrie (using only benchmarks which used prototype 
 * implementation in package ctrie2)
 * 
 * running the benchmark, example usage:
 * scala -Dsz=1000000 -cp classes/ AfterDeleteCHM 1 
 */


object AfterDeleteCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  def run() {
    val chm = new ConcurrentHashMap[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) chm.put(e(i), e(i))
    for (i <- 0 until sz) chm.remove(e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}


object AfterDeleteSkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  def run() {
    val skiplist = new ConcurrentSkipListMap[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) skiplist.put(e(i), e(i))
    for (i <- 0 until sz) skiplist.remove(e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}

/*
object AfterDeleteCtrie extends Benchmark {
  def run() {
    val ctrie = new ConcurrentTrie[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) ctrie.insert(e(i), e(i))
    for (i <- 0 until sz) ctrie.remove(e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}
*/


object AfterDeleteCtrie2 extends Benchmark {
  def run() {
    val ctrie = new ConcurrentTrie[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) ctrie.update(e(i), e(i))
    for (i <- 0 until sz) ctrie.remove(e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}


object MemoryCHM extends Benchmark {
  import java.util.concurrent.ConcurrentHashMap
  
  def run() {
    val chm = new ConcurrentHashMap[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) chm.put(e(i), e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}


object MemorySkipList extends Benchmark {
  import java.util.concurrent.ConcurrentSkipListMap
  
  def run() {
    val skiplist = new ConcurrentSkipListMap[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) skiplist.put(e(i), e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}

/*
object MemoryCtrie extends Benchmark {
  def run() {
    val ctrie = new ConcurrentTrie[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) ctrie.insert(e(i), e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}
*/

object MemoryCtrie2 extends Benchmark {
  def run() {
    val ctrie = new ConcurrentTrie[Elem, Elem]
    val e = elems
    
    for (i <- 0 until sz) ctrie.update(e(i), e(i))
    
    Runtime.getRuntime.gc()
    while (true) {}
  }
}


