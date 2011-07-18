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

  def factorsWithExponents(n: Long) = {
    var primes = List[(Long, Int)]()
    var c = n
    var p = 2L
    while (p < c) {
      var e = 0
      if (c % p == 0) {
        do {
          c = c / p
          e += 1
        } while (c % p == 0)
        primes = (p, e) :: primes
      }
      p += 1
    }
    if (c != 1)
      primes = (p, 1) :: primes
    primes
  }

  def factors(n: Long) = factorsWithExponents(n).map(_._1)

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

  def pow(n: Long, e: Int) = BigInt(n).pow(e).toLong

  class FactorMap(impl: Map[Long,Int] = Map[Long,Int]()) {
    def add(t: (Long,Int)) = t match {
      case (f, e) => new FactorMap(impl updated (f, e max impl.getOrElse(f, 0)) )
    }

    def getP0wedList = impl.map(p => pow(p._1, p._2))
  }

  def euler5 = {
    var fm = new FactorMap
    for (i <- 20 to 1 by -1) {
      factorsWithExponents(i).foreach { pair =>
        fm = fm.add(pair)
      }
    }
    fm.getP0wedList.foldLeft(1L)(_*_)
  }

  def euler6 = pow((1 to 100).sum,2) - (1 to 100).map(pow(_, 2)).sum

  def isPrime(ps: Stream[Long], n: Long): Boolean = {
    ps.takeWhile(p => p*p <= n).foldLeft(true)((a,p) => a && n % p != 0)
  }
  val primes: Stream[Long] = 2L #:: Stream.iterate(3L)(_+2).filter(isPrime(primes, _))
  def isPrime(n: Long): Boolean = isPrime(primes, n)

  def euler7 = primes(10000) // 0-based index

  def euler8 = {
    val theString = """
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
""".trim()
    """\s+""".r.split(theString).mkString.sliding(5).map(_.map(_.toString.toInt).product).max
  }
}
