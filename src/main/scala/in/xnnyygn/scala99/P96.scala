package in.xnnyygn.scala99

// a template for answer
object P96 {

  // method goes here
  def isIdentifier(s: String): Boolean = {
    import shared.SyntaxRule._
    import shared.ParserException

    // BNF
    def identifier = letter + repeat(
      optional(liternal('-')) + (letter or digit)
    )
    def letter = liternal(c => c >= 'a' && c <= 'z', "letter")
    def digit = liternal(c => c >= '0' && c <= '9', "digit")

    try {
      identifier(s, 0)
      true
    } catch {
      case e: ParserException => println(e); false
    }
  }

}