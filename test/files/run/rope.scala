/** Test the Scala implementation of trait <code>scala.collection.immutable.Rope</code>.
 *  
 *  @author Heather Miller
 */
import scala.collection.immutable.Rope

object Test {
  def main(args: Array[String]) {

    val rope = Rope("the quick red fox jumps over the lazy brown dog".toArray,10)

    // test length
    assert(rope.length == 47, "rope.length FAIL")

    // test apply
    assert(rope(4) == 'q', "rope.apply FAIL")

    // test concat
    val r1 = "the q"
    val r2 = "uick red f"
    val r3 = "ox j"
    val r4 = "umps o"
    val r5 = "ver the laz"
    val r6 =  "y brown dog"
    assert((r1 concat r2 concat r3 concat r4 concat r5 concat r6).toList == rope.toList, "rope.concat FAIL")

    // split test # 1
    val rope2 = Rope("there is a cat.".toArray, 2)
    val splitrope1 = Rope("there is".toArray)
    val splitrope2 = Rope(" a cat.".toArray)

    val sp1 = Rope("the".toArray, 2)
    val sp2 = Rope("re is".toArray, 2)
    val sp3 = Rope(" a c".toArray, 2)
    val sp4 = Rope("at.".toArray, 2)

    val rope3 = (sp1 concat ((sp2 concat sp3) concat sp4))
    assert(rope3.toList == rope2.toList, "rope.concat FAIL (in split test #1)")

//    val (splitrope3, splitrope4) = rope3.split(8)
//    println("output of split: (" + splitrope3.toList + ", " + splitrope4.toList + ")")
//    assert( (splitrope3, splitrope4) == (splitrope1.toList, splitrope2.toList), "rope3.split FAIL (in split test #1)")
  }
}
