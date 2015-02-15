package in.xnnyygn.scala99.shared

trait SyntaxRule[A] extends ((String, Int) => (A, Int)) {
  def canStartsWith(c: Char): Boolean
  def +[B](r2: SyntaxRule[B]) = new SyntaxRule[(A, B)] {
    def canStartsWith(c: Char) = SyntaxRule.this.canStartsWith(c)
    def apply(s: String, p: Int): ((A, B), Int) = {
      val (va, p2) = SyntaxRule.this.apply(s, p)
      val (vb, p3) = r2.apply(s, p2)
      ((va, vb), p3)
    }
  }
  def map[B](f: A => B) = new SyntaxRule[B] {
    def canStartsWith(c: Char) = SyntaxRule.this.canStartsWith(c)
    def apply(s: String, p: Int): (B, Int) = {
      val (v, p2) = SyntaxRule.this.apply(s, p)
      (f(v), p2)
    }
  }
}

object SyntaxRule {
  
  def liternal(c: Char): SyntaxRule[Char] = liternal(_ == c, c.toString)
  def liternal(f: Char => Boolean, d: String = "") = new SyntaxRule[Char] {
    def canStartsWith(c: Char): Boolean = f(c)
    def apply(s: String, p: Int): (Char, Int) = s(p) match {
      case c if f(c) => (c, p + 1)
      case c => throw new IllegalStateException(s"unable to apply liternal rule $d, position $p, current $c")
    }
    override def toString = "liternal"
  }
  def optional[A](r: SyntaxRule[A]) = new SyntaxRule[Option[A]] {
    def canStartsWith(c: Char): Boolean = r.canStartsWith(c)
    def apply(s: String, p: Int): (Option[A], Int) = {
      // println(s"optional $p")
      // println(s"r $r")
      if(r.canStartsWith(s(p))) {
        val (v, p2) = r.apply(s, p)
        (Some(v), p2)
      } else (None, p)
    }
    override def toString = "optional"
  }
  // call by name is required
  def repeat[A](r: => SyntaxRule[A]) = new SyntaxRule[List[A]] {
    def canStartsWith(c: Char): Boolean = r.canStartsWith(c)
    def apply(s: String, p: Int): (List[A], Int) = {
      // println(s"repeat $p")
      optional(r).apply(s, p) match {
        case (None, p2) => (Nil, p2)
        case (Some(v), p2) => {
          val (vs, p3) = apply(s, p2)
          (v :: vs, p3)
        }
      }
    }
    override def toString = "repeat"
  }
}