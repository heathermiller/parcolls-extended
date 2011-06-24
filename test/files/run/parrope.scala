/** Test the Scala implementation of trait
 *  <code>scala.collection.parallel.immutable.ParRope</code>.
 *  
 *  @author Heather Miller
 */
import scala.collection.parallel.immutable.ParRope
import scala.collection.immutable.Rope

object Test {
  def main(args: Array[String]) {

    val rope = Rope("the quick red".toArray) // length == 13
    val parRope = new ParRope(rope)

    // test splitters
    val zeeSplitter = parRope.splitter
    val splitters = zeeSplitter.split

    val firstSplitter = splitters(0)
    var res: String = ""
    while (firstSplitter.hasNext)
      res = res + firstSplitter.next
    assert(res == "the qu", "split FAIL")

    // test combiners
    val comb1 = ParRope.newCombiner[Char]
    val comb2 = ParRope.newCombiner[Char]
    comb1 += 't'
    comb1 += 'h'
    comb1 += 'e'
    comb1 += ' '
    comb2 += 'q'
    comb2 += 'u'
    comb2 += 'i'
    comb2 += 'c'
    comb2 += 'k'
    val comb = comb1.combine(comb2)
    val combinedRope = comb.result
    
    var combinedString = ""
    val combIter = combinedRope.splitter
    while (combIter.hasNext)
      combinedString += combIter.next
    assert(combinedString == "the quick", "combine FAIL")
  }
}

