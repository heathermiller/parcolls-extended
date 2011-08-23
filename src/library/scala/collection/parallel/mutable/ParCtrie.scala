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

import scala.collection.generic.CanCombineFrom
import scala.collection.parallel.Combiner

class ParCtrie[K, V](var ctrie: ConcurrentTrie[K, V]) extends ParMap[K, V]
  with ParMapLike[K, V, ParCtrie[K, V], ConcurrentTrie[K, V]] {
  
  type SCPI = SignalContextPassingIterator[CtrieSplitter]
  
  override def empty = {
    val emptyTrie = new ConcurrentTrie[K, V]
    new ParCtrie[K, V](emptyTrie)
  }
  
  override def newCombiner = {
    ParCtrie.newCombiner(new ConcurrentTrie[K, V])
  }
  
  def seq = ctrie

  def clear: Unit = {
    ctrie = new ConcurrentTrie[K, V]
  }
    	
  def -=(key: K) = {
    ctrie.-=(key)
    this
  }
  
  def +=(keyvalue: (K, V)) = {
	ctrie.put(keyvalue._1, keyvalue._2)
    this
  }
  
  def put(key: K, value: V) =
    ctrie.put(key, value)
  
  def splitter = {
    // obtain read-only snapshot
    val snap = ctrie.readOnlySnapshot()
    // splitter with size of snapshot
    new CtrieSplitter(snap.size, snap.asInstanceOf[ConcurrentTrie[K, V]]) with SCPI
  }
  
  def get(key: K) =
    ctrie.get(key)
  
  def size = {
    // obtain read-only snapshot
    val snap = ctrie.readOnlySnapshot()
    snap.size
  }
  
  protected[this] override def cbfactory = {
    var shared = new ConcurrentTrie[K, V]
    () => ParCtrie.newCombiner(shared)
  }

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

}

class ParCtrieCombiner[K, V](var ctrie: ConcurrentTrie[K,V]) extends Combiner[(K,V), ParCtrie[K,V]] {
  // ParCtrie combiner, because concurrent, should just use internal ctrie to add to.
  def +=(keyvalue: (K, V)) = {
    ctrie.put(keyvalue._1, keyvalue._2)
    this
  }
  
  def result = new ParCtrie(ctrie)
  
  def clear = {
    ctrie = new ConcurrentTrie
  }
  
  def combine[N <: (K, V), NewTo >: ParCtrie[K,V]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = other
  
  def size = sys.error("not yet implemented")
}

object ParCtrie { 
  
  implicit def canBuildFrom[K,V]: CanCombineFrom[ParCtrie[K,V], (K,V), ParCtrie[K,V]] =
    new CanCombineFrom[ParCtrie[K,V], (K,V), ParCtrie[K,V]] {
	  @volatile var sharedCtrie = new ConcurrentTrie[K,V]
      def apply(from: ParCtrie[K,V]): Combiner[(K,V), ParCtrie[K,V]] = newCombiner[K,V](sharedCtrie)
      def apply(): Combiner[(K,V), ParCtrie[K,V]] = newCombiner[K,V](sharedCtrie)
    }
  
  def newCombiner[K,V](sharedCtrie: ConcurrentTrie[K,V]): Combiner[(K,V), ParCtrie[K,V]] = new ParCtrieCombiner[K,V](sharedCtrie)
  
}
