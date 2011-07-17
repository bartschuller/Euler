
object Euler {
  def euler1 = (1 to 999).filter(x => x % 3 == 0 || x % 5 == 0).sum

  def fib(n: Int): Int = {
    def fibs(n: Int): (Int, Int) = n match {
      case _ if n <= 0 => (0,1)
      case _ => fibs(n-1) match {
        case (a,b) => (b, a+b)
      }
    }
    fibs(n)._1
  }

  def fib_iterator = Iterator.iterate((1L,2L))(p => (p._2, p._1+p._2)).map(_._1)
  def euler2 = fib_iterator.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum

  
}
