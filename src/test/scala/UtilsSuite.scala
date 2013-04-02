import org.scalatest.FunSuite
import ru.org.codingteam.loprog._

class UtilsSuite extends FunSuite {
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

  test("mapVarName") {
    // foo(X, bar(Y), baz)
    val term = Functor("foo",
      List(
        Variable("X"),
        Functor("bar", List(Variable("Y"))),
        Functor("baz", List())
      )
    )

    // foo(X::derp, bar(Y::derp), baz)
    val answer = Functor("foo",
      List(
        Variable("X::derp"),
        Functor("bar", List(Variable("Y::derp"))),
        Functor("baz", List())
      )
    )

    val result = Utils.mapVarName(term, _ + "::derp")

    assert(answer === result)
  }

  test("createGenerator") {
    val generator = Utils.createGenerator
    assert(generator() === "_G1")
    assert(generator() === "_G2")
    assert(generator() === "_G3")
  }

  test("scopePredicate") {
    // foo(X, Y) :-
    //   bar(Z), baz(X, Z, Y).
    val predicate = Predicate(Functor("foo", List(Variable("X"), Variable("Y"))),
      List(
        Functor("bar", List(Variable("Z"))),
        Functor("baz", List(Variable("X"), Variable("Z"), Variable("Y")))
      )
    )

    // foo(FOO1, FOO2) :-
    //   bar(FOO3), baz(FOO1, FOO3, FOO2).
    val answer = Predicate(Functor("foo", List(Variable("FOO1"), Variable("FOO2"))),
      List(
        Functor("bar", List(Variable("FOO3"))),
        Functor("baz", List(Variable("FOO1"), Variable("FOO3"), Variable("FOO2")))
      )
    )

    var index = 0;

    val generator = () => {
      index += 1
      s"FOO$index"
    }

    val result = Utils.scopePredicate(predicate, generator)
    assert(result === answer)
  }
}
