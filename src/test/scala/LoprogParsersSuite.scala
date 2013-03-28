import org.scalatest.FunSuite
import ru.org.codingteam.loprog._

class LoprogParsersSuite extends FunSuite {
  test("variable parser") {
    val result = LoprogParsers.parse(LoprogParsers.variable, "Foo")
    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === Variable("Foo"))
  }

  test("functor parser") {
    val answer = Functor("foo",
      List(
        Functor("a", List()),
        Functor("g", List(Variable("X"))),
        Variable("Y")
      )
    )
    val result = LoprogParsers.parse(LoprogParsers.functor, "foo(a, g(X), Y)")
    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === answer)
  }

  test("predicate parser (with body and arguments)") {
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

  test("predicate parser (without body and arguments)") {
    val answer = Predicate(Functor("foo", List()), List())
    val result = LoprogParsers.parse(LoprogParsers.predicate, "foo.")

    assert(result.successful, "Unsuccessful parse result")
    assert(result.get === answer)
  }

  test("sourceCode parser") {
    val answer = List(
      // foo.
      Predicate(Functor("foo", List()), List()),

      // bar(X) :-
      //   herp,
      //   derp(X).
      Predicate(Functor("bar", List(Variable("X"))),
        List(
          Functor("herp", List()),
          Functor("derp", List(Variable("X")))
        )
      )
    )

    val result = LoprogParsers.parse(
      LoprogParsers.sourceCode,
      """|foo.
         |
         |bar(X) :-
         |  herp,
         |  derp(X).""".stripMargin
    )

    assert(result.successful)
    assert(result.get === answer)
  }

  test("query parser") {
    val answer = List(
      // f(X, a)
      Functor("f", List(Variable("X"), Functor("a", List()))),

      // g(b, Y)
      Functor("g", List(Functor("b", List()), Variable("Y")))
    )

    val result = LoprogParsers.parse(
      LoprogParsers.query,
      "f(X, a), g(b, Y)."
    )

    assert(result.successful)
    assert(result.get === answer)
  }
}
