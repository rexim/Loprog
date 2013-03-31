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

  test("collectVars") {
    val terms = List(
      // foo(X, a, bar(Y))
      Functor("foo",
        List(
          Variable("X"),
          Functor("a", List()),
          Functor("bar", List(Variable("Y")))
        )
      ),

      // baz(X, Z)
      Functor("baz",
        List(
          Variable("X"),
          Variable("Z")
        )
      )
    )

    val answer = Set("X", "Y", "Z")

    val result = Utils.collectVars(terms)

    assert(result === answer)
  }

  test("showValue (non-recursive)") {
    val bindings = Map(
      // X = Y
      "X" -> Variable("Y"),

      // Y = foo(a, Z)
      "Y" -> Functor("foo",
        List(
          Functor("a", List()),
          Variable("Z")
        )
      )
    )

    val answer = "foo(a, Z)"
    val result = Utils.showValue("X", bindings)

    assert(result === answer)
  }

  test("showValue (recursive)") {
    val bindings = Map(
      // X = Y
      "X" -> Variable("Y"),

      // Y = foo(a, X)
      "Y" -> Functor("foo",
        List(
          Functor("a", List()),
          Variable("X")
        )
      )
    )

    val answer = "foo(a, **)"
    val result = Utils.showValue("X", bindings)

    assert(result == answer)
  }
}
