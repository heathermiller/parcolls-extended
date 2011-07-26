/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package parallel.mutable

import scala.collection.mutable.ConcurrentTrie
import scala.collection.mutable.CtrieIterator

import scala.collection.parallel.BucketCombiner
import scala.collection.mutable.DefaultEntry

//needed?
//import scala.collection.mutable.ArrayBuffer
//import scala.collection.generic.CanCombineFrom // used to return a combiner

class ParCtrie[K, V](ctrie: ConcurrentTrie[K, V]) extends ParMap[K, V]
  with ParMapLike[K, V, ParCtrie[K, V], ConcurrentTrie[K, V]] {
  
  type SCPI = SignalContextPassingIterator[CtrieSplitter]
  
  override def empty = {
    val emptyTrie = new ConcurrentTrie[K, V]
    new ParCtrie[K, V](emptyTrie)
  }
  
  override def newCombiner = sys.error("not yet implemented.")
 
  def seq = sys.error("not yet implemented")
  def clear = sys.error("not yet implemented")
  def -=(key: K) = this //in trait ParMapLike of type (key: K)ParCtrie.this.type is not defined
  def +=(keyvalue: (K, V)) = this //in trait ParMapLike of type (kv: (K, V))ParCtrie.this.type is not defined
  def put(key: K, value: V) = None //in trait ParMapLike of type (key: K, value: V)Option[V] is not defined
  def splitter = sys.error("not yet implemented") //in trait ParIterableLike of type => scala.collection.parallel.IterableSplitter[(K, V)] is not defined
  def get(key: K) = None  //in trait GenMapLike of type (key: K)Option[V] is not defined
  def size = sys.error("not yet implemented") //in trait GenTraversableLike of type => Int is not defined

  class CtrieSplitter(szestimate: Int, ct: ConcurrentTrie[K, V])
  extends CtrieIterator[K, V](ct) with ParIterator {
    self: SCPI =>
    
    def remaining: Int = szestimate 
    
    override def remainingEstimate: Int = szestimate 
    
    def dup = throw new UnsupportedOperationException // not using views
    
    def split: Seq[CtrieSplitter] = subdivide.map { // probably won't use LNodes
      case ci: CtrieIterator[K, V] =>
        val cs = new CtrieSplitter(szestimate / 2, ct) with SCPI
        cs.stack = ci.stack
        cs.stackpos = ci.stackpos
        cs.depth = ci.depth
        cs.current = ci.current
        cs
    }
 
  }

  /*
  def apply(i: Int): T = rope(i)

  def length: Int = rope.length

  override def seq: Rope[T] = rope

  override protected[this] def newCombiner = ParRope.newCombiner[T]
  
  type SCPI = SignalContextPassingIterator[ParRopeIterator]
  
  def parallelIterator: ParIterator = new ParRopeIterator with SCPI

  def splitter: SeqSplitter[T] = new ParRopeIterator with SCPI
  
  class ParRopeIterator(var i: Int = 0, val until: Int = rope.length) extends super.ParIterator {
    me: SignalContextPassingIterator[ParRopeIterator] =>
      
      def hasNext = i < until
    
      def next = {
        val c = rope.apply(i)
        i += 1
        c
      }
    
      def remaining = until - i
      
      def dup = new ParRopeIterator(i, until) with SCPI
      
      def split: Seq[ParIterator] = if (remaining > 1) {
        val div = remaining / 2
        Seq(new ParRopeIterator(i, i + div) with SCPI, new ParRopeIterator(i + div, until) with SCPI)  
        } else Seq(this)
      
      def psplit(sizes: Int*): Seq[ParIterator] = {
        var hold: Int = i
        val parts = mutable.ArrayBuffer[ParRopeIterator]()
        for (sz <- sizes) {
          parts += new ParRopeIterator(hold, hold + sz.min(until-hold)) with SCPI
          hold += sz.min(until-hold)
        }
        parts
      }
      
    }
    */

}

private class ParCtrieCombiner[K, V](bucketnumber: Int) extends BucketCombiner[(K, V), ParCtrie[K, V], DefaultEntry[K, V], ParCtrieCombiner[K, V]](bucketnumber) {
  
  def +=(keyvalue: (K, V)) = sys.error("not yet implemented.")
  def result = sys.error("not yet implemented.")
  /*
   * Needed: (?)
   * def +=
   * def result 
   * def size
   * def clear
   * def combine
   */
  }

object ParCtrie {
  /*implicit def canBuildFrom[T]: CanCombineFrom[ParRope[T], T, ParRope[T]] =
    new CanCombineFrom[ParRope[T], T, ParRope[T]] {
      def apply(from: ParRope[T]): Combiner[T, ParRope[T]] = newCombiner[T]
      def apply(): Combiner[T, ParRope[T]] = newCombiner[T]
    }
  
  def newBuilder[T]: Combiner[T, ParRope[T]] = newCombiner[T]
  def newCombiner[T]: Combiner[T, ParRope[T]] = new ParRopeCombiner[T]()
  */
}

/*class ParCtrie[K, V](szest: Int) extends mutable.ParMap[K, V] {
  val seq = new ConcurrentTrie[K, V]
  
  def clear() = throw new UnsupportedOperationException
  
  final def +=(kv: (K, V)) = {
    seq += kv
    this
  }
  
  final def remove(k: K): Option[V] = seq.remove(k)
  
  final def -=(k: K) = {
    seq -= k
    this
  }
  
  def splitter: IterableSplitter[(K, V)] =
    new CtrieSplitter(szest, seq.readOnlySnapshot().asInstanceOf[ConcurrentTrie[K, V]]) with SCPI
  
  def get(k: K): Option[V] = seq.get(k)
  
  def put(key: K, value: V): Option[V] = seq.put(key, value)
  
  def size = szest
  
  type SCPI = SignalContextPassingIterator[CtrieSplitter]
  
  class CtrieSplitter(szestimate: Int, ct: ConcurrentTrie[K, V])
  extends CtrieIterator[K, V](ct) with ParIterator {
  self: SCPI =>
    def remaining = szestimate // not using these ops
    def dup = throw new UnsupportedOperationException // not using views
    def split: Seq[CtrieSplitter] = subdivide.map { // probably won't use LNodes
      case ci: CtrieIterator[K, V] =>
        val cs = new CtrieSplitter(szestimate / 2, ct) with SCPI
        cs.stack = ci.stack
        cs.stackpos = ci.stackpos
        cs.depth = ci.depth
        cs.current = ci.current
        cs
    }
  }
  
}*/