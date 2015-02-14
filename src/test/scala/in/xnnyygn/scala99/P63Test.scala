package in.xnnyygn.scala99

import org.specs2.mutable._

// a template for test
class P63Test extends Specification {
  
  import shared._

  "P63" should {
    "completeBinaryTree" in {
      Tree.completeBinaryTree(6, 'x) must_== Node('x, Node('x, Node('x), Node('x)), Node('x, Node('x), End))
    }
  }
}