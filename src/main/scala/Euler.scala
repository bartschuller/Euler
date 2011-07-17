import io.Source
import java.io.File

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

  def factors(n: Long) = {
    var primes = List[Long]()
    var c = n
    var p = 2L
    while (p < c) {
      if (c % p == 0) {
        primes = p :: primes
        do {
          c = c / p
        } while (c % p == 0)
      }
      p += 1
    }
    if (c != 1)
      primes = p :: primes
    primes
  }

  def euler3 = factors(600851475143L).max

  def euler4 = {
    val pals = for {
      a <- 100 to 999
      b <- 100 to 999
      p = a*b
      r = p.toString.reverse
      if p.toString == r
    } yield p
    pals.max
  }
}
