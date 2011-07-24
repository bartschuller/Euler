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

  def primeFactorsWithExponents(n: Long) = {
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

  def primeFactors(n: Long) = primeFactorsWithExponents(n).map(_._1)

  def euler3 = primeFactors(600851475143L).max

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
      primeFactorsWithExponents(i).foreach { pair =>
        fm = fm.add(pair)
      }
    }
    fm.getP0wedList.foldLeft(1L)(_*_)
  }

  def euler6 = pow((1 to 100).sum,2) - (1 to 100).map(pow(_, 2)).sum

  class IntPrimes {
    def isPrime(ps: Stream[Int], n: Int): Boolean = {
      ps.takeWhile(p => p*p <= n).foldLeft(true)((a,p) => a && n % p != 0)
    }
    val primes: Stream[Int] = 2 #:: Stream.iterate(3)(_+2).filter(isPrime(primes, _))
    def isPrime(n: Int): Boolean = isPrime(primes, n)
  }

  class LongPrimes {
    def isPrime(ps: Stream[Long], n: Long): Boolean = {
      ps.takeWhile(p => p*p <= n).foldLeft(true)((a,p) => a && n % p != 0)
    }
    val primes: Stream[Long] = 2L #:: Stream.iterate(3L)(_+2).filter(isPrime(primes, _))
    def isPrime(n: Long): Boolean = isPrime(primes, n)
  }

  def euler7 = new IntPrimes().primes(10000) // 0-based index

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

  def euler9 = {
    val ans =
      for {b <- 2 to 1000
           a <- 1 to b
           c = 1000 - a - b if c*c == a*a + b*b } yield a*b*c
    ans.head
  }

  def euler10 = new LongPrimes().primes takeWhile(_ < 2000000) sum

  def euler11 = {
    val theString = """
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
""".trim()
    val m = Matrix.fromString(theString)
    m.directions.flatMap(_.sliding(4)).map(_.product).max
  }

  def triangleNumbers = Iterator.iterate((1,1))((p =>(p._1+1,p._1+p._2+1))).map(_._2)

  def numFactors(n: Long) = primeFactorsWithExponents(n).map(_._2+1).product

  def euler12 = triangleNumbers find(numFactors(_) > 500) get
}
