

import collection._
import scala.concurrent._
import collection.parallel.mutable.ParArray

// test custom thread pools

object Test {
  
    
  def main(args: Array[String]) {
    val array = ParArray[Int]()
    val parArray = array ++ (1 to 10000)

    testDefaultPool(parArray)
    testFJPool(parArray)
    testCachedPool(parArray)
    testFixedPool(parArray)
  }
  
  def testFJPool(parArray: ParArray[Int]) {
    //Thread.sleep(5000)  // so we can see this thread pool in jvisualvm
    
    val fjPool = TaskRunners.makeForkJoinPool(2)
    parArray.taskRunner = fjPool
    
    val mapped = parArray.map( (x: Int) => x*2)
    assertSeq(mapped)
  }

  def testCachedPool(parArray: ParArray[Int]) {
    //Thread.sleep(5000)  // so we can see this thread pool in jvisualvm
    
    val cachedPool = TaskRunners.makeCachedThreadPool
    parArray.taskRunner = cachedPool
    
    val mapped = parArray.map( (x: Int) => { x*2 })
    assertSeq(mapped)

    cachedPool.shutdown()
  }

  def testFixedPool(parArray: ParArray[Int]) {
    //Thread.sleep(5000)  // so we can see this thread pool in jvisualvm
    
    val fixedPool = TaskRunners.makeFixedThreadPool(2)
    parArray.taskRunner = fixedPool
    
    val mapped = parArray.map( (x: Int) => x*2)
    assertSeq(mapped)

    fixedPool.shutdown()
  }

  def testDefaultPool(parArray: ParArray[Int]) {
    //Thread.sleep(5000)  // so we can see this thread pool in jvisualvm
    
    val mapped = parArray.map( (x: Int) => x*2)
    assertSeq(mapped)
  }  
  
  def assertSeq[T](pc: parallel.ParIterable[T]) = assert(pc.seq == pc)
  
}
