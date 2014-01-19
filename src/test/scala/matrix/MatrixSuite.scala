package matrix

import org.scalatest.FunSuite
import org.scalatest.prop._
import org.scalacheck.Prop._
import org.scalacheck._

class MatrixSuite extends FunSuite with Checkers {

  test("should update and get element of matrix by index, like m(i, j) = x") {
    val m = Matrix(2, 2)
    check(forAll(Gen.choose(0, 1), Gen.choose(0, 1), Gen.choose(0.0, 100.0)) {
      (i, j, number) => m(i, j) = number
        number == m(i, j)
    })
  }
}
