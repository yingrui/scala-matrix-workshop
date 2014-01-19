package matrix

trait Matrix {

  val row: Int
  val col: Int

  import Matrix.seqToArray
  def row(i: Int): Matrix = Matrix(for(j <- 0 until col) yield apply(i, j))

  def col(j: Int): Matrix = Matrix(for(i <- 0 until row) yield apply(i, j))

  def apply(i: Int, j: Int): Double

  def update(i: Int, j: Int, value: Double)

  def +(n: Double): Matrix

  def -(n: Double): Matrix

  def x(n: Double): Matrix

  def /(n: Double): Matrix

  def == (other: Matrix): Boolean = {
    val value = for (i <- 0 until row; j <- 0 until col) yield Math.abs(apply(i, j) - other(i, j))
    row == other.row && col == other.col && value.sum < 1E-10
  }
}

object Matrix {

  implicit def seqToArray(seq: Seq[Double]):Array[Double] = seq.toArray

  def apply(elements: Double*): Matrix = new DenseMatrix(1, elements.length, elements)

  def apply(elements: Array[Double]): Matrix = new DenseMatrix(1, elements.length, elements)

  def apply(row: Int, col: Int): Matrix = new DenseMatrix(row, col, new Array[Double](row * col))

  def apply(row: Int, col: Int, elements: Array[Double]): Matrix = new DenseMatrix(row, col, elements)

}

class DenseMatrix(val row: Int, val col: Int, elements: Array[Double]) extends Matrix {

  def apply(i: Int, j: Int): Double = elements(i * col + j)

  def update(i: Int, j: Int, value: Double): Unit = elements(i * col + j) = value

  def +(n: Double): Matrix = Matrix(row, col, elements.map(_ + n))

  def -(n: Double): Matrix = this + -n

  def x(n: Double): Matrix = Matrix(row, col, elements.map(_ * n))

  def /(n: Double): Matrix = Matrix(row, col, elements.map(_ / n))
}