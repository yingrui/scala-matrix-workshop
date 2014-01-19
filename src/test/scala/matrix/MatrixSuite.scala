package matrix

import org.scalatest.FunSuite
import org.scalatest.prop._
import org.scalacheck.Prop._
import org.scalacheck._

class MatrixSuite extends FunSuite with Checkers {

  val matrixGen: Gen[Matrix] = {
    val indexGen = for (
      i <- Gen.choose(1, 10);
      j <- Gen.choose(1, 10)
    ) yield (i, j)
    indexGen.flatMap(t =>
      Gen.listOfN(t._1 * t._2, Gen.choose(-100.0, 100.0))
        .map(list => Matrix(t._1, t._2, list.toArray))
    )
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
}
