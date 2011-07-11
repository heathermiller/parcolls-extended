/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package immutable

import generic._
import mutable.Builder
import mutable.ArrayBuffer
import parallel.immutable.ParRope
import util.control.Breaks._

trait Rope[T] extends Seq[T]
              with GenericTraversableTemplate[T, Rope]
              with SeqLike[T, Rope[T]] {
  // prepend
  override def +:[B >: T, That](elem: B)(implicit bf: CanBuildFrom[Rope[T], B, That]): That = {
    val leafToAdd = Leaf[T](Array(elem.asInstanceOf[AnyRef]))
    InnerNode(leafToAdd, this).asInstanceOf[That]
  }  

  // append
  override def :+[B >: T, That](elem: B)(implicit bf: CanBuildFrom[Rope[T], B, That]): That = {
    val leafToAdd = Leaf[T](Array(elem.asInstanceOf[AnyRef]))
    InnerNode(this, leafToAdd).asInstanceOf[That]
  }

  def prependAndRebalance(elem: T): Rope[T] = {
    val prependedRope = this.+:(elem)
    if (!prependedRope.isBalanced) {
      prependedRope.rebalance
    } else
      prependedRope
  }

  def appendAndRebalance(elem: T): Rope[T] = {
    val appendedRope = this :+ elem
    if (!appendedRope.isBalanced) {
      appendedRope.rebalance
    } else
      appendedRope
  }


  def apply(i: Int): T

  // used by rebalance(). Implementation with LeafIterator should be faster than using findLeafAt.
  private def collectLeaves: ArrayBuffer[Rope[T]] = {
    val buffer = new ArrayBuffer[Rope[T]]
    val iter = new LeafIterator()

    while (iter.hasNext)
    {
      val foundLeaf = iter.next
      
      // to keep it immutable, we have to copy.
      val copiedArray = Array.ofDim[AnyRef](foundLeaf.length)
      Array.copy(foundLeaf.array, 0, copiedArray, 0, foundLeaf.length)
      val leafToInsert = Leaf[T](copiedArray)
      
      buffer += leafToInsert
    }
    buffer
  }

  // used by split(). Implementation with LeafIterator should be faster than using findLeafAt.
  private def collectLeavesForSplit(i: Int): (ArrayBuffer[Rope[T]], ArrayBuffer[Rope[T]]) = {
    var offset = 0
    val first = new ArrayBuffer[Rope[T]]
    val second = new ArrayBuffer[Rope[T]]
    var buffer = first
    val iter = new LeafIterator()

    while (iter.hasNext)
    {
      val foundLeaf = iter.next //returns the next leaf in the sequence using the stack.
      
      // to keep it immutable, we have to copy.
      val copiedArray = Array.ofDim[AnyRef](foundLeaf.length)
      Array.copy(foundLeaf.array, 0, copiedArray, 0, foundLeaf.length)
      val leafToInsert = Leaf[T](copiedArray)

      if (i >= offset && i <= offset + leafToInsert.length - 1) { //then i is in the found leaf.
        val firstArray = Array.ofDim[AnyRef](i-offset)
        val secondArray = Array.ofDim[AnyRef](foundLeaf.length-(i-offset))
        Array.copy(foundLeaf.array, 0, firstArray, 0, i-offset)
        Array.copy(foundLeaf.array, i-offset, secondArray, 0, foundLeaf.length-(i-offset))
        val firstLeaf = Leaf[T](firstArray)
        val secondLeaf = Leaf[T](secondArray)
        buffer += firstLeaf
        buffer = second
        buffer += secondLeaf
      } else { // just put the entire leaf into the buffer
        buffer += leafToInsert
      }
      offset = offset + leafToInsert.length
    }
    (first, second)
  }

  // used by take(). Implementation with LeafIterator should be faster than using findLeafAt.
  private def collectLeavesForTake(i: Int): ArrayBuffer[Rope[T]] = {
    var offset = 0
    val buffer = new ArrayBuffer[Rope[T]]
    val iter = new LeafIterator()

    breakable {
      while (iter.hasNext)
      {
        val foundLeaf = iter.next //returns the next leaf in the sequence using the stack.
        
        // to keep it immutable, we have to copy.
        val copiedArray = Array.ofDim[AnyRef](foundLeaf.length)
        Array.copy(foundLeaf.array, 0, copiedArray, 0, foundLeaf.length)
        val leafToInsert = Leaf[T](copiedArray)

        if (i >= offset && i <= offset + leafToInsert.length - 1) { //then i is in the found leaf.
          val firstArray = Array.ofDim[AnyRef](i-offset)
          Array.copy(foundLeaf.array, 0, firstArray, 0, i-offset)
          val firstLeaf = Leaf[T](firstArray)
          buffer += firstLeaf
          break
        } else { // just put the entire leaf into the buffer
          buffer += leafToInsert
        }
        offset = offset + leafToInsert.length
      }
    }
    buffer
  }

  // used by drop(). Implementation with LeafIterator should be faster than using findLeafAt.
  private def collectLeavesForDrop(i: Int): ArrayBuffer[Rope[T]] = {
    var offset = 0
    val buffer = new ArrayBuffer[Rope[T]]
    val iter = new LeafIterator()

    val (firstLeaf, leafOffset) = iter.startAt(i)
    offset = leafOffset

    val splitArray = Array.ofDim[AnyRef](firstLeaf.length-(i-offset))
    Array.copy(firstLeaf.array, i-offset, splitArray, 0, firstLeaf.length-(i-offset))
    val splitLeaf = Leaf[T](splitArray)
    buffer += splitLeaf

    while (iter.hasNext) // just put the rest of the leaves into the buffer
    {
      val nextLeaf = iter.next

      // to keep it immutable, we have to copy.
      val copiedArray = Array.ofDim[AnyRef](nextLeaf.length)
      Array.copy(nextLeaf.array, 0, copiedArray, 0, nextLeaf.length)
      val leafToInsert = Leaf[T](copiedArray)

      buffer += leafToInsert
    }
    buffer
  }

  override def drop(i: Int): Rope[T] = Rope.buildFromLeaves(this.collectLeavesForDrop(i))

  protected[immutable] def findLeafAt(i: Int): Leaf[T]

  private[immutable] def findWithStack(i: Int, stack: mutable.Stack[InnerNode[T]], offset: Int = 0): (Leaf[T], Int)

  override def foreach[U](f: T => U): Unit = {
    val leafIterator = new LeafIterator
    while (leafIterator.hasNext) {
      val leaf = leafIterator.next
      leaf.array.foreach((elem: AnyRef) => f(elem.asInstanceOf[T]))
    }
  }

  def concat(that: Rope[T]): Rope[T]
  override def companion: GenericCompanion[Rope] = Rope

  private def fibonacci(n: Int): Int = if (n == 0 || n == 1) 1 else fibonacci(n-1) + fibonacci(n-2)

  def isBalanced: Boolean = {
    val n = this.depth
    length >= fibonacci(n+2)
  }     

  def length: Int
  override def par = new ParRope(this)

  def rebalance: Rope[T] = Rope.buildFromLeaves(this.collectLeaves)

  override def splitAt(i: Int): (Rope[T], Rope[T]) = {
    val (first, second) = this.collectLeavesForSplit(i)
    (Rope.buildFromLeaves(first), Rope.buildFromLeaves(second))
  }

  def subseq(from: Int, to: Int): Rope[T]

  override def take(i: Int): Rope[T] = Rope.buildFromLeaves(this.collectLeavesForTake(i))

  /* Used for avoiding too many inner nodes. */
  protected[immutable] def isShortLeaf: Boolean = false
  def iterator: Iterator[T] = new RopeIterator

  protected[immutable] def depth: Int

  class RopeIterator(var i: Int = 0, val until: Int = Rope.this.length) extends Iterator[T] {
    def hasNext = i < until
    def next: T = {
      i += 1
      Rope.this(i-1)
    }
  }

  private class LeafIterator extends Iterator[Leaf[T]] {
    var stack = mutable.Stack[InnerNode[T]]()
    var currentNode = Rope.this
    var offset = 0
    var i = 0

    def hasNext = i < Rope.this.length

    def next = {
      while (stack.nonEmpty && i >= currentNode.length + offset) {//we have to go up
        offset -= stack.top.left.length
        currentNode = stack.pop
      }

      // at this point, we know that i < currentNode.length
      val (foundLeaf, _) = currentNode.findWithStack(i, stack)
      if (stack.nonEmpty && stack.top.left == foundLeaf) offset = i
      if (stack.nonEmpty) currentNode = stack.pop
      i += foundLeaf.length
      foundLeaf
    }

    // has to be called before `next`. Returns the first leaf and its offset.
    def startAt(pos: Int): (Leaf[T], Int) = {
      val (foundLeaf, leafOffset) = currentNode.findWithStack(pos, stack)
      
      offset = if (stack.nonEmpty) {
        if (stack.top.left == foundLeaf) leafOffset
        else leafOffset - stack.top.left.length
      } else 0
      
      if (stack.nonEmpty) currentNode = stack.pop
      i = leafOffset + foundLeaf.length
      (foundLeaf, leafOffset)
    }

  }// end LeafIterator
}// end Rope



// LEAF
case class Leaf[T] private[immutable] (val array: Array[AnyRef], shortLeaf: Int = Rope.defaultShortLeaf) extends Rope[T] {

  def apply(i: Int): T = array(i).asInstanceOf[T]

  def concat(that: Rope[T]): Rope[T] = {
    if (this.isShortLeaf && that.isShortLeaf) {
      val thatLeaf = that.asInstanceOf[Leaf[T]]
      Leaf((this.array ++ thatLeaf.array).toArray, shortLeaf)
    }
    else
      new InnerNode(this, that)
  }
  
  protected[immutable] def depth: Int = 0

  protected[immutable] def findLeafAt(i: Int): Leaf[T] = this

  private[immutable] def findWithStack(i: Int, stack: mutable.Stack[InnerNode[T]], offset: Int): (Leaf[T], Int) = (this, offset)
  
  override def isShortLeaf: Boolean = this.length <= shortLeaf

  def length: Int = array.length

  def subseq(from: Int, to: Int): Rope[T]  = sys.error("not implemented yet.")

} // end Leaf


// INNER NODE
case class InnerNode[T] private[immutable] (var left: Rope[T], var right: Rope[T]) extends Rope[T] {

  val length = left.length + right.length

  def apply(i: Int): T = {
    if (left.length <= i)
      right.apply(i-left.length)
    else if (left.length > i)
      left.apply(i)
    else 
      sys.error("Index out of bounds.")
  }

  def concat(that: Rope[T]): Rope[T] = {
    if (right.isShortLeaf && that.isShortLeaf) {
      val newRightLeaf = right.concat(that)
      new InnerNode(left, newRightLeaf)
    } else
      new InnerNode(this, that)
  }

  protected[immutable] def depth: Int = 1 + math.max(left.depth, right.depth)

  protected[immutable] def findLeafAt(i: Int): Leaf[T] = {
    if (left.length <= i)
      right.findLeafAt(i-left.length)
    else if (left.length > i)
      left.findLeafAt(i)
    else 
      sys.error("Index out of bounds.")
  }

  private[immutable] def findWithStack(i: Int, stack: mutable.Stack[InnerNode[T]], offset: Int): (Leaf[T], Int) = {
    stack.push(this)
    if (left.length <= i)
      right.findWithStack(i-left.length, stack, offset + left.length)
    else if (left.length > i)
      left.findWithStack(i, stack, offset)
    else 
      sys.error("Index out of bounds.")
  }

/*
  // split returns two ropes, one containing the first i elements, and another containing the rest
  def split(i: Int): (Rope[T], Rope[T]) = {
    var directionChange: List[Either[InnerNode[T],InnerNode[T]]] = List()
    var previous: InnerNode[T] = this
    var current: Rope[T] = this
    var splitNode: InnerNode[T] = this
    var index: Int = i
    var previousDirection: Option[Either[InnerNode[T],InnerNode[T]]] = None

    // searches for node to split and builds list of direction changes
    while (!current.isInstanceOf[Leaf[T]]) {
      if (left.length <= index) {
        previous = previousDirection match {
          case Some(Left(node)) => 
            directionChange ::= Left(node)
            node
          case Some(Right(node)) => node
          case None => this
        }
        previousDirection = Some(Right(current.asInstanceOf[InnerNode[T]]))
        splitNode = current.asInstanceOf[InnerNode[T]]
        current = right
        index = index -left.length
      } else if (left.length> index) {
        // this is the case where we go right.
        // the pattern match checks to see if this is a change in direction.
        previous = previousDirection match {
          case Some(Left(node)) => node
          case Some(Right(node)) => 
            directionChange ::= Right(node)
            node
          case None => this
        }
        previousDirection = Some(Left(current.asInstanceOf[InnerNode[T]]))
        splitNode = current.asInstanceOf[InnerNode[T]]
        current = left
      } else
        sys.error("Index out of bounds.")
    }


    // if i is in a leaf, split that leaf into two new leaves
    val currentLeaf = current.asInstanceOf[Leaf[T]].array
    if (index != 0) {
//      println("index was found to be within a leaf, so we have to first split the leaf.")
      val newLeft = Array.ofDim[AnyRef](index)
      val newRight = Array.ofDim[AnyRef](currentLeaf.length - index)
      Array.copy(currentLeaf, 0, newLeft, 0, index)
      Array.copy(currentLeaf, index, newRight, 0, currentLeaf.length - index)
      val newSplitNode = new InnerNode[T](Leaf[T](newLeft), Leaf[T](newRight))

      if (current == splitNode.left) {
        directionChange ::= Left(splitNode)
        splitNode.left = newSplitNode
      }
      else {
        directionChange ::= Right(splitNode)
        splitNode.right = newSplitNode
      }
         splitNode = newSplitNode
    }

    // do the splitting
    // start here
    var floatingRope: Rope[T] = null
    if (current == splitNode.left && index == 0) 
      directionChange match {
        case List() => return (Rope(), this)
        case List(Left(node)) => return (Rope(), this)
        case Right(node)::rest => 
          splitNode = node
          directionChange = rest
      }

    // initializing floatingRope
    directionChange match {
      case Left(leftnode)::Right(rightnode)::rest => 
        floatingRope = rightnode.right
        leftnode.left = splitNode.right
        rightnode.right = splitNode.left
        directionChange = rest
      case Right(rightnode)::Left(leftnode)::rest =>           
        floatingRope = leftnode.left
        rightnode.right = splitNode.left
        leftnode.left = splitNode.right
        directionChange = rest
      case List(Left(leftnode)) => 
        floatingRope = leftnode.left
        leftnode.left = splitNode.right
        previous.right = splitNode.left
        return (floatingRope, this)
      case List(Right(rightnode)) =>
        floatingRope = rightnode.right
        rightnode.right = splitNode.left
        previous.left = splitNode.right
        return (this, floatingRope)
    }

    // iterating through the list of direction changes
    var finalLeftRope: Rope[T] = null
    var finalRightRope: Rope[T] = null
    while (!directionChange.isEmpty) {
      directionChange match {
        case Left(leftnode)::Right(rightnode)::rest => 
          val oldFloatingRope = floatingRope
          floatingRope = rightnode.right
          rightnode.right = leftnode.left
          leftnode.left = oldFloatingRope
          directionChange = rest 
          if (rest.isEmpty) {
            finalLeftRope = floatingRope 
            finalRightRope = this // this should be finished
          }
        case Right(rightnode)::Left(leftnode)::rest =>           
          val oldFloatingRope = floatingRope
          floatingRope = leftnode.left
          leftnode.left = rightnode.right
          rightnode.right = oldFloatingRope
          directionChange = rest 
          if (rest.isEmpty) {
            finalLeftRope = this
            finalRightRope = floatingRope // this should be finished
          }
        case List(Left(leftnode)) => 
          val oldFloatingRope = floatingRope
          floatingRope = leftnode.left
          leftnode.left = oldFloatingRope
          directionChange = List()
          finalLeftRope = floatingRope
          finalRightRope = this // this should be finished
        case List(Right(rightnode)) =>
          val oldFloatingRope = floatingRope
          floatingRope = rightnode.right
          rightnode.right = oldFloatingRope
          directionChange = List()
          finalLeftRope = this 
          finalRightRope = floatingRope // this should be finished
      }
    }
    (finalLeftRope, finalRightRope)
    
    // end here

  } // end def split
*/
  def subseq(from: Int, to: Int): Rope[T]  = sys.error("not implemented yet.")

}// end InnerNode


final private class RopeBuilder[T](shortLeaf: Int = Rope.defaultShortLeaf) extends Builder[T, Rope[T]] {
  
  val chain = ArrayBuffer(ArrayBuffer[AnyRef]())
  var last = chain(0)
  
  def +=(elem: T) = {
    if (last.length < shortLeaf)
      last += elem.asInstanceOf[AnyRef]
    else {
      val arrayBufferToInsert = ArrayBuffer[AnyRef]()
      arrayBufferToInsert += elem.asInstanceOf[AnyRef]
      last = arrayBufferToInsert
      chain += last
    }
    this
  }
  
  def result: Rope[T] = {    
    Rope.buildFromBuffer(chain.map(c => c.toArray))
  }
  
  def size = chain.foldLeft(0)(_ + _.length)
  
  def clear = {
    chain.clear
    chain += new ArrayBuffer[AnyRef]()
    last = chain(0)
  }

} // end RopeBuilder

object Rope extends SeqFactory[Rope] {
  
  val defaultShortLeaf = 32 // the default length of leaves

  private[immutable] val BF = new GenericCanBuildFrom[Nothing] {
    override def apply() = newBuilder[Nothing]
  }
  
  @inline implicit def canBuildFrom[T]: CanBuildFrom[Coll, T, Rope[T]] =
    BF.asInstanceOf[CanBuildFrom[Coll, T, Rope[T]]]
  def newBuilder[T]: Builder[T, Rope[T]] = new RopeBuilder[T]
  @inline override def empty[T]: Rope[T] = Rope[T]()

  def apply(s: String): Rope[Char] = Rope(s.toArray)

  def apply[T](): Rope[T] = Leaf(Array.ofDim[AnyRef](0))

  def apply[T](array: Array[T], shortLeaf: Int = defaultShortLeaf): Rope[T] = {
    //Leaf(array map (el => el.asInstanceOf[AnyRef]), shortLeaf)
    val buf = new ArrayBuffer[Array[AnyRef]]
    var i = 0
    while (i < array.length) {
      // length of the subarray
      val len =
        if (i + shortLeaf > array.length) array.length - i
        else shortLeaf

      val subarray = Array.ofDim[AnyRef](len)
      Array.copy(array, i, subarray, 0, len)

      buf += subarray
      i += len
    }
    buildFromBuffer(buf)
  }

  // Builds a new *balanced* rope from an array buffer of leaves.
  protected[immutable] def buildFromLeaves[T](leaves: ArrayBuffer[Rope[T]]): Rope[T] = 
    leaves.length match {
      case 0 => Rope()
      case 1 => leaves(0)
      case 2 => new InnerNode(leaves(0), leaves(1))
      case other => 
        // if length of leaves is odd...
        if (leaves.length % 2 == 1) {
          var idx = 0
          while (idx < leaves.length) {
            if (idx + 1 < leaves.length) {
              leaves(idx) = new InnerNode(leaves(idx), leaves(idx+1))
              leaves.remove(idx+1)
            }              
            idx += 1           
          }
          leaves(leaves.length-2) = new InnerNode(leaves(leaves.length-2),leaves(leaves.length-1))
          leaves.remove(leaves.length-1)
        }
      
        // at this point, the length of leaves must be even.
        while (leaves.length > 1) {
          var idx = 0
          while (idx < leaves.length - 1) {
            leaves(idx) = new InnerNode(leaves(idx), leaves(idx+1))
            leaves.remove(idx+1)
            idx += 1 
          }                
        }

        // return rope stored in leaves(0)  
        leaves(0)
    }

/*
 * applySeq used in Combiner and Builder
*/
  def buildFromBuffer[T](arrays: ArrayBuffer[Array[AnyRef]]): Rope[T] = {
    val leaves: ArrayBuffer[Rope[T]] = arrays map (a => Leaf[T](a))
    buildFromLeaves(leaves)
  }

}// end object Rope

