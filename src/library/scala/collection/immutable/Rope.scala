/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package immutable

trait Rope[T] extends Iterable[T] {
  
  def apply(i: Int): T
  def concat(that: Rope[T]): Rope[T]
  def delete(i: Int): Rope[T] 
  def insert(i: Int, r: Rope[T]): Rope[T]

  def length: Int
  def rebalance: Rope[T]
  def split(i: Int): (Rope[T], Rope[T])

  def subseq(from: Int, to: Int): Rope[T]

  /* Used for avoiding 
   */
  protected[immutable] def isShortLeaf: Boolean = false
  def iterator: Iterator[T] = new RopeIterator

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
class Leaf[T] private[immutable] (val array: Array[AnyRef], shortLeaf: Int = 10) extends Rope[T] {

  def apply(i: Int): T = array(i).asInstanceOf[T]

  def concat(that: Rope[T]): Rope[T] = {
    if (this.isShortLeaf && that.isShortLeaf) {
      val thatLeaf = that.asInstanceOf[Leaf[T]]
      new Leaf((this.array ++ thatLeaf.array).toArray, shortLeaf)
    }
    else
      new InnerNode(this, that)
  }

  def delete(i: Int): Rope[T]  = sys.error("not implemented yet.")
  def insert(i: Int, r: Rope[T]): Rope[T]  = sys.error("not implemented yet.")
  
  override def isShortLeaf: Boolean = this.length <= shortLeaf

  def length: Int = array.length
  def rebalance: Rope[T]  = sys.error("not implemented yet.")
  def split(i: Int): (Rope[T], Rope[T])  = sys.error("not implemented yet.")
  def subseq(from: Int, to: Int): Rope[T]  = sys.error("not implemented yet.")

} // end Leaf


// INNER NODE
class InnerNode[T] private[immutable] (var left: Rope[T], var right: Rope[T]) extends Rope[T] {

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

  def delete(i: Int): Rope[T]  = sys.error("not implemented yet.")
  def insert(i: Int, r: Rope[T]): Rope[T]  = sys.error("not implemented yet.")
  def rebalance: Rope[T] = sys.error("not implemented yet.")

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
      val newSplitNode = new InnerNode[T](new Leaf[T](newLeft),new Leaf[T](newRight))

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

  def subseq(from: Int, to: Int): Rope[T]  = sys.error("not implemented yet.")

}// end InnerNode


object Rope {

  def apply[T](): Rope[T] = new Leaf(Array.ofDim[AnyRef](0))

  def apply[T](array: Array[T], shortLeaf: Int = 10): Rope[T] = {
    //val anyRefArray = Array.ofDim[AnyRef](array.length)
    new Leaf(array map (el => el.asInstanceOf[AnyRef]), shortLeaf)
  }

  def applySeq[T](arrays: Seq[Array[AnyRef]]): Rope[T] =
    arrays.length match {
      case 0 => Rope()
      //case 1 => Rope(arrays(0) map (el => el.asInstanceOf[T]))
      case 2 => new InnerNode(new Leaf(arrays(0)), new Leaf(arrays(1)))
      case other => 
                var temp = new InnerNode[T](new Leaf(arrays(0)), new Leaf(arrays(1)))

                for (array <- arrays.drop(2))
                  temp = new InnerNode(temp, new Leaf(array))
                temp
    }

}// end object Rope


