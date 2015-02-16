package in.xnnyygn.scala99.shared

import scala.util.control.NonFatal

class ParserException(msg: String) extends RuntimeException(msg)

trait SyntaxRule[+A] extends ((String, Int) => (A, Int)) {
  def +[B](r2: => SyntaxRule[B]) = new SyntaxRule[(A, B)] {
    def apply(s: String, p: Int): ((A, B), Int) = {
      val (va, p2) = SyntaxRule.this.apply(s, p)
      val (vb, p3) = r2.apply(s, p2)
      ((va, vb), p3)
    }
  }
  def +>[B](r2: => SyntaxRule[B]) = new SyntaxRule[B] {
    def apply(s: String, p: Int): (B, Int) = {
      val (_, p2) = SyntaxRule.this.apply(s, p)
      r2.apply(s, p2)
    }
  }
  def <+(r2: => SyntaxRule[_]) = new SyntaxRule[A] {
    def apply(s: String, p: Int): (A, Int) = {
      val (va, p2) = SyntaxRule.this.apply(s, p)
      val (_, p3) = r2.apply(s, p2)
      (va, p3)
    }
  }
  def or[B >: A](r2: => SyntaxRule[B]) = new SyntaxRule[B] {
    def apply(s: String, p: Int): (B, Int) = {
      try {
        SyntaxRule.this.apply(s, p)
      } catch {
        case _ : ParserException => r2.apply(s, p)
        case e if NonFatal(e) => throw e
      }
    }
  }
  def map[B](f: A => B) = new SyntaxRule[B] {
    def apply(s: String, p: Int): (B, Int) = {
      val (v, p2) = SyntaxRule.this.apply(s, p)
      (f(v), p2)
    }
  }
}

object SyntaxRule {
  
  def liternal(c: Char): SyntaxRule[Char] = liternal(_ == c, c.toString)
  def liternal(f: Char => Boolean, d: String = "") = new SyntaxRule[Char] {
    def apply(s: String, p: Int): (Char, Int) = {
      if(p < 0 || p >= s.length) throw new ParserException("encounter eof")
      s(p) match {
        case c if f(c) => (c, p + 1)
        case c => throw new ParserException(s"unable to apply liternal rule $d, position $p, current $c")
      }
    }
    override def toString = "liternal"
  }
  def optional[A](r: => SyntaxRule[A]) = new SyntaxRule[Option[A]] {
    def apply(s: String, p: Int): (Option[A], Int) = {
      try {
        val (v, p2) = r.apply(s, p)
        (Some(v), p2)
      } catch {
        case _ : ParserException => (None, p)
        case e if NonFatal(e) => throw e
      }
    }
    override def toString = "optional"
  }
  // call by name is required
  def repeat[A](r: => SyntaxRule[A]) = new SyntaxRule[List[A]] {
    val optr = optional(r)
    def apply(s: String, p: Int): (List[A], Int) = optional(r).apply(s, p) match {
      case (None, p2) => (Nil, p2)
      case (Some(v), p2) => {
        val (vs, p3) = apply(s, p2)
        (v :: vs, p3)
      }
    }
    override def toString = "repeat"
  }
}