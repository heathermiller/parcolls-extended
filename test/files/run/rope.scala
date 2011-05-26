/** Test the Scala implementation of trait <code>scala.collection.immutable.Rope</code>.
 *
 *  @author Heather Miller
 */
import scala.collection.immutable.Rope

object Test {
  def main(args: Array[String]) {

    val rope = Rope("the quick red fox jumps over the lazy brown dog")
    
    // length
    assert(rope.length == 47, "rope.length FAIL")

    // apply
    assert(rope(4) == 'q', "rope.apply FAIL")

    // concat
    val r1 = "the q"
    val r2 = "uick red f"
    val r3 = "ox j"
    val r4 = "umps o"
    val r5 = "ver the laz"
    val r6 =  "y brown dog"
    assert((r1 concat r2 concat r3 concat r4 concat r5 concat r6).toList == rope.toList, "rope.concat FAIL")
  }
}
