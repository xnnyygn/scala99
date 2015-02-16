package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P73Test extends Specification {
  
  import shared.MTree
  import MTree._

  "P73" should {
    // test goes here
    "lispyTree 1" in {
      MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree must_== "(a (b c))"
    }
    "lispyTree 2" in {
      "afg^^c^bd^e^^^".lispyTree must_== "(a (f g) c (b d e))"
    }
    "fromLispyString" in {
      val t = MTree.fromLispyString("(a (f g) c (b d e))")
      // println(t)
      t.value must_== 'a'
    }
  }
}