/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable


trait Rope[T] /*private[immutable]*/ extends Iterable[T] {
  
  def apply(i: Int): T
  def concat(that: Rope[T]): Rope[T]
  def delete(i: Int): Rope[T] 
  def insert(i: Int, r: Rope[T]): Rope[T]
  def isShortLeaf: Boolean = false
  def iterator: Iterator[T] = new RopeIterator
  def length: Int
  def rebalance: Rope[T]
  def split(i: Int): (Rope[T], Rope[T])
  def subseq(from: Int, to: Int): Rope[T]

  //Helpers
  protected def depth: Int = 0 //TODO

  class RopeIterator(var i: Int = 0, val until: Int = Rope.this.length) extends Iterator[T] {
    def hasNext = i < until
    def next: T = {
      i += 1
      Rope.this(i-1)
    }
  }

}// end Rope



// LEAF
class Leaf[T: Manifest](val array: Array[T], shortLeaf: Int) extends Rope[T] {

  def apply(i: Int):T = array(i).asInstanceOf[T]

  def concat(that: Rope[T]): Rope[T] = {
    if (this.isShortLeaf && that.isShortLeaf) {
      val thatLeaf = that.asInstanceOf[Leaf[T]]
      new Leaf((this.array ++ thatLeaf.array).toArray, shortLeaf)
    }
    else
      new InnerNode(this, that)
  }

  def delete(i: Int): Rope[T]  = error("not implemented yet.")
  def insert(i: Int, r: Rope[T]): Rope[T]  = error("not implemented yet.")
  
  override def isShortLeaf: Boolean = this.length <= shortLeaf

  def length: Int = array.length
  def rebalance: Rope[T]  = error("not implemented yet.")
  def split(i: Int): (Rope[T], Rope[T])  = error("not implemented yet.")
  def subseq(from: Int, to: Int): Rope[T]  = error("not implemented yet.")

} // end Leaf


// INNER NODE
class InnerNode[T](left: Rope[T], right: Rope[T]) extends Rope[T] {

  val length = left.length + right.length

  def apply(i: Int): T = {
    if (left.length <= i)
      right.apply(i-length)
    else if (left.length > i)
      left.apply(i)
    else 
      error("Index out of bounds.")
  }

  def concat(that: Rope[T]): Rope[T] = {
    if (right.isShortLeaf && that.isShortLeaf) {
      val newRightLeaf = right.concat(that)
      new InnerNode(left, newRightLeaf)
    } else
      new InnerNode(this, that)
  }

  def delete(i: Int): Rope[T]  = error("not implemented yet.")
  def insert(i: Int, r: Rope[T]): Rope[T]  = error("not implemented yet.")
  def rebalance: Rope[T] = error("not implemented yet.")
  def split(i: Int): (Rope[T], Rope[T])  = error("not implemented yet.")
  def subseq(from: Int, to: Int): Rope[T]  = error("not implemented yet.")

}// end InnerNode


object Rope {

  def apply[T: Manifest](seq: Seq[T], shortLeaf: Int = 10): Rope[T] = {
    new Leaf(seq.toArray, shortLeaf)
  }

}// end object Rope
