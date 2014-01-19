package matrix

trait Matrix {

  def apply(i: Int, j: Int): Double

  def update(i: Int, j: Int, value: Double)

}

object Matrix {

  def apply(row: Int, col: Int): Matrix = new DenseMatrix

}

class DenseMatrix extends Matrix {

  def apply(i: Int, j: Int): Double = ???

  def update(i: Int, j: Int, value: Double): Unit = ???
}