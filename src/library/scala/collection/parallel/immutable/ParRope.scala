/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package parallel.immutable

import scala.collection.immutable.Rope
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.Combiner
import scala.collection.parallel.SeqSplitter
import scala.collection.generic.CanCombineFrom // used to return a combiner

class ParRope[T](rope: Rope[T]) extends ParSeq[T]
  with parallel.ParSeqLike[T, ParRope[T], Vector[T]] {
  
  def apply(i: Int): T = rope(i)

  def length: Int = rope.length

  /* This should return a sequential Rope, once Rope is integrated into
   * the sequential collections framework.
   * 
   * As a first step we can return a Vector.
   */
  def seq = {
    val vb = new collection.immutable.VectorBuilder[T]()
    val iter = this.parallelIterator
    while (iter.hasNext)
      vb += iter.next
    vb.result
  }

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

}

private class ParRopeCombiner[T]() extends parallel.Combiner[T, ParRope[T]] {
  
  val chain = mutable.ArrayBuffer(ArrayBuffer[AnyRef]())
  var last = chain(0)
  
  def +=(elem: T) = {
    last += elem.asInstanceOf[AnyRef]
    this
  }
  
  def result: ParRope[T] = {    
    new ParRope(Rope.applySeq(chain.map(c => c.toArray).toList))
  }
  
  def size = chain.foldLeft(0)(_ + _.length)
  
  def clear = {
    chain.clear
    chain += new ArrayBuffer[AnyRef]()
    last = chain(0)
  }
  
  def combine[N <: T, NewTo >: ParRope[T]](other: Combiner[N, NewTo]): Combiner[N, NewTo] =
    if (this ne other) {
      val that = other.asInstanceOf[ParRopeCombiner[T]]
      
      chain ++= that.chain
      last = chain.last
      
      this
    } else this
}

object ParRope {
  implicit def canBuildFrom[T]: CanCombineFrom[ParRope[T], T, ParRope[T]] =
    new CanCombineFrom[ParRope[T], T, ParRope[T]] {
      def apply(from: ParRope[T]): Combiner[T, ParRope[T]] = newCombiner[T]
      def apply(): Combiner[T, ParRope[T]] = newCombiner[T]
    }
  
  def newBuilder[T]: Combiner[T, ParRope[T]] = newCombiner[T]
  def newCombiner[T]: Combiner[T, ParRope[T]] = new ParRopeCombiner[T]()
}
