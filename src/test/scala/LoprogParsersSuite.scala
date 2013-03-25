import org.scalatest.FunSuite
import com.github.rexim._

class LoprogParsersSuite extends FunSuite {
  test("atom parser") {
    val result = LoprogParsers.parse(LoprogParsers.atom, "foo")
    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === Atom("foo"))
  }

  test("variable parser") {
    val result = LoprogParsers.parse(LoprogParsers.variable, "Foo")
    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === Variable("Foo"))
  }

  test("functor parser (without arguments)") {
    val answer = Functor("foo", List())
    val result = LoprogParsers.parse(LoprogParsers.functor, "foo")
    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === answer)
  }

  test("functor parser (with arguments)") {
    val answer = Functor("foo",
      List(
        Atom("a"),
        Functor("g", List(Variable("X"))),
        Variable("Y")
      )
    )
    val result = LoprogParsers.parse(LoprogParsers.functor, "foo(a, g(X), Y)")
    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === answer)
  }

  test("predicate parser") {
    val answer = Predicate(Functor("p", List(Variable("A"), Variable("B"))),
      List(
        Functor("f", List(Variable("A"))),
        Functor("g", List(Variable("B")))
      )
    )

    val result = LoprogParsers.parse(
      LoprogParsers.predicate,
      "p(A, B) :- f(A), g(B)."
    )

    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === answer)
  }
}
