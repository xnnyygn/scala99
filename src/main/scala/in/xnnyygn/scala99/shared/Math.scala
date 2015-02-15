package in.xnnyygn.scala99.shared

object Math {

  def pow2(n: Int): Int = {
    if(n == 0) 1
    else pow2(n - 1) << 1
  }
  
}