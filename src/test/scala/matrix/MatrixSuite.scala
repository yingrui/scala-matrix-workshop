package matrix

import org.scalatest.FunSuite
import org.scalatest.prop._
import org.scalacheck.Prop._
import org.scalacheck._

class MatrixSuite extends FunSuite with Checkers {

  def matrixGen: Gen[Matrix] = {
    val indexGen = for (
      i <- Gen.choose(1, 10);
      j <- Gen.choose(1, 10)
    ) yield (i, j)
    indexGen.flatMap(t =>
      Gen.listOfN(t._1 * t._2, Gen.choose(-100.0, 100.0))
        .map(list => Matrix(t._1, t._2, list.toArray))
    )
  }

  def matrixGen(row: Int, col: Int): Gen[Matrix] = {
    Gen.listOfN(row * col, Gen.choose(-100.0, 100.0))
      .map(list => Matrix(row, col, list.toArray))
  }

  test("should update and get element of matrix by index, like m(i, j) = x") {
    val m = Matrix(2, 2)
    check(forAll(Gen.choose(0, 1), Gen.choose(0, 1), Gen.choose(0.0, 100.0)) {
      (i, j, number) => m(i, j) = number
        number == m(i, j)
    })
  }

  test("should support +/- operator") {
    check(forAll(matrixGen, Gen.choose(-100.0, 100.0)) { (m, num) =>
      val n = m + num
      m == n - num
    })
  }

  test("should support +/- operator of matrix") {
    check(forAll(matrixGen(2, 3), matrixGen(2, 3)) { (m, n) =>
      val diff = m - n
      (n + diff) == m
    })
  }

  test("should support multiple/divide operator") {
    check(forAll(matrixGen, Gen.choose(0.01, 100.0)) { (m, num) =>
      val n = m x num
      m == n / num
    })
  }

  test("should return itself when multiple with E") {
    check(forAll(matrixGen) { m =>
      val n = m x Matrix.E(m.col)
      m == n
    })
  }

  test("should get result of multiple with matrix") {
    val m = Matrix(2, 2, Array(1.0, 2.0, 3.0, 4.0))
    assert(Matrix(2, 2, Array(7.0, 10.0, 15.0, 22.0)) == (m x m))
  }

  test("should return result of dot production") {
    val m = Matrix(3.0, 4.0)
    assert(5.0 == Math.sqrt(m * m), "3 x 3 + 4 x 4 = 5 x 5")
  }

  test("should return row as matrix") {
    val m = Matrix(2, 2, Array(1.0, 2.0, 3.0, 4.0))
    assert(Matrix(1.0, 2.0) == m.row(0))
    assert(Matrix(3.0, 4.0) == m.row(1))
  }

  test("should return column as matrix") {
    val m = Matrix(2, 2, Array(1.0, 2.0, 3.0, 4.0))
    assert(Matrix(1.0, 3.0) == m.col(0))
    assert(Matrix(2.0, 4.0) == m.col(1))
  }
}
