import org.scalatest.FunSuite
import ru.org.codingteam.loprog._

class UtilsSuite extends FunSuite {
  test("addPrefixToVars") {
    // foo(X, a, bar(Y))
    val term = Functor("foo",
      List(
        Variable("X"),
        Functor("a", List()),
        Functor("bar", List(Variable("Y")))
      )
    )

    // foo(prefix::X, a, bar(prefix::Y))
    val answer = Functor("foo",
      List(
        Variable("prefix::X"),
        Functor("a", List()),
        Functor("bar", List(Variable("prefix::Y")))
      )
    )

    val result = Utils.addPrefixToVars("prefix", term)

    assert(result === answer)
  }
}
