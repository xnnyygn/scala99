package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P68Test extends Specification {
  
  import P67._

  "P68" should {
    // test goes here
    "preorder" in {
      val s = "a(b(d,e),c(,f(g,)))"
      Tree.fromString(s).preorder must_== List('a', 'b', 'd', 'e', 'c', 'f', 'g')
    }
    "inorder" in {
      val s = "a(b(d,e),c(,f(g,)))"
      Tree.fromString(s).inorder must_== List('d', 'b', 'e', 'a', 'c', 'g', 'f')
    }
    "preInTree" in {
      Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f')).toString must_==
        "a(b(d,e),c(,f(g,)))"
    }
  }
}