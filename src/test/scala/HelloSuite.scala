
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class HelloSuite extends FunSuite with Checkers {

  test("should return hello world!") {
    val greeting = new Greeting
    check(forAll {
      name: String =>
        greeting.hi(name).endsWith(s"$name!")
    })
  }

}
