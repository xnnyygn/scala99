package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P69Test extends Specification {
  
  import P67.Tree
  
  "P69" should {
    // test goes here
    "fromDotString" in {
      val s = "a(b(d,e),c(,f(g,)))"
      val t = Tree.fromString(s)
      val ds = t.toDotstring
      Tree.fromDotString(ds).toDotstring must_== ds
    }
  }
}