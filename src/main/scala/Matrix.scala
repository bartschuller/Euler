import scala.math.min

class Matrix(val m: Int, val n: Int) {
  val vals = new Array[Array[Int]](m)
  0 until m foreach { i => vals(i) = new Array[Int](n) }

  def apply(i: Int, j: Int) = vals(i)(j)
  def update(i: Int, j: Int, v: Int) { vals(i)(j) = v }
  override def toString = vals.map(_.map("%02d".format(_)).mkString(" ")).mkString("\n")
  def rows = vals.map(_.toIndexedSeq).toIndexedSeq
  def columns = (0 until n).map(j=>(0 until m).map(i=>vals(i)(j)))
  def downright1 = (0 until m).map(i=>(0 until min(m-i,n)).map(j=>vals(i+j)(j)))
  def downright2 = (1 until n).map(j=>(0 until min(n-j,m)).map(i=>vals(i)(i+j)))
  def upright1 = (0 until m).map(i=>(0 until min(i+1,n)).map(j=>vals(i-j)(j)))
  def upright2 = (1 until n).map(j=>(0 until min(n-j,m)).map(i=>vals(m-i-1)(i+j)))
  def directions = rows ++ columns ++ downright1 ++ downright2 ++ upright1 ++ upright2
}

object Matrix {
  def fromString(s: String) = {
    val splitR = """\s+""".r
    val lines = s.lines.toIterable
    val m = lines.size
    val n = splitR.split(lines.head).size
    val matrix = new Matrix(m,n)
    for { (row,i) <- lines.zipWithIndex
          (v, j) <- splitR.split(row).zipWithIndex }
      matrix(i, j) = v.toInt
    matrix
  }
}
