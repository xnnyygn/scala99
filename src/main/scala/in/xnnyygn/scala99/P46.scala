package in.xnnyygn.scala99

import scala.language.implicitConversions
// a template for answer
object P46 {

  // for problem P47
  implicit def booleanToP46Boolean(a: Boolean): P47Boolean = new P47Boolean(a)

  class P47Boolean(a: Boolean) {
    def and(b: Boolean): Boolean = P46.and(a, b)
    def or(b: Boolean): Boolean = P46.or(a, b)
  }

  // method goes here
  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def not(x: Boolean): Boolean = !x

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
  def equ(a: Boolean, b: Boolean): Boolean = a == b

  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    println("A\tB\tresult")
    println("true\ttrue\t" + f(true, true))
    println("true\tfalse\t" + f(true, false))
    println("false\ttrue\t" + f(false, true))
    println("false\tfalse\t" + f(false, false))
  }
}