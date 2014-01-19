package matrix

trait Matrix {

  def apply(i: Int, j: Int): Double

  def update(i: Int, j: Int, value: Double)

}

object Matrix {

  def apply(row: Int, col: Int): Matrix = new DenseMatrix(row, col, new Array[Double](row * col))

}

class DenseMatrix(val row: Int, val col: Int, elements: Array[Double]) extends Matrix {

  def apply(i: Int, j: Int): Double = elements(i * col + j)

  def update(i: Int, j: Int, value: Double): Unit = elements(i * col + j) = value
}