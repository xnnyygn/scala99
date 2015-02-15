package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P67Test extends Specification {
  
  import P67._

  "P67" should {
    // test goes here
    "toString simple" in {
      Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString must_==
        "a(b(d,e),c(,f(g,)))"
    }
    "fromString" in {
      val s = "a(b(d,e),c(,f(g,)))"
      Tree.fromString(s).toString must_== s
    }
  }
}