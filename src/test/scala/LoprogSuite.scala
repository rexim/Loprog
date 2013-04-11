import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import ru.org.codingteam.loprog._

class LoprogSuite extends FunSuite {
  test("unify") {
    // p(X, f(Y), a)
    val left = Functor("p",
      List(
        Variable("X"),
        Functor("f", List(Variable("Y"))),
        Functor("a", List())
      )
    )

    // p(a, f(a), Y)
    val right = Functor("p",
      List(
        Functor("a", List()),
        Functor("f", List(Functor("a", List()))),
        Variable("Y")
      )
    )

    // X = a; Y = a.
    val answer = Map(
      Variable("X") -> Functor("a", List()),
      Variable("Y") -> Functor("a", List())
    )

    // ?- p(X, f(Y), a) = p(a, f(a), Y).
    val bindings = Loprog.unify(left, right, Map())

    assert(bindings === Some(answer))
  }

  test("visitSolutions") {
    val program = List(
      // p(a).
      Predicate(Functor("p", List(Functor("a", List()))), List()),

      // p(X) :- q(X), r(X).
      Predicate(Functor("p", List(Variable("X"))),
        List(
          Functor("q", List(Variable("X"))),
          Functor("r", List(Variable("X")))
        )),

      // p(X) :- u(X).
      Predicate(Functor("p", List(Variable("X"))),
        List(
          Functor("u", List(Variable("X")))
        )),

      // q(X) :- s(X).
      Predicate(Functor("q", List(Variable("X"))),
        List(
          Functor("s", List(Variable("X")))
        )),

      // r(a).
      Predicate(Functor("r", List(Functor("a", List()))), List()),

      // r(b).
      Predicate(Functor("r", List(Functor("b", List()))), List()),

      // s(a).
      Predicate(Functor("s", List(Functor("a", List()))), List()),

      // s(b).
      Predicate(Functor("s", List(Functor("b", List()))), List()),

      // s(c).
      Predicate(Functor("s", List(Functor("c", List()))), List()),

      // u(d).
      Predicate(Functor("u", List(Functor("d", List()))), List())
    )

    // ?- p(X).
    val query = List(Functor("p", List(Variable("X"))))

    val solutions = new ListBuffer[Map[Variable, Term]]()

    Loprog.visitSolutions(program, query,
      (bindings: Map[Variable, Term]) => {
        solutions.append(bindings)
        Next
      })

    val answer = List(
      Some(Functor("a", List())),
      Some(Functor("a", List())),
      Some(Functor("b", List())),
      Some(Functor("d", List()))
    )

    assert(solutions.toList.map(_.get(Variable("X"))) === answer)
  }

  test("showValue (non-recursive)") {
    val bindings = Map(
      // X = Y
      Variable("X") -> Variable("Y"),

      // Y = foo(a, Z)
      Variable("Y") -> Functor("foo",
        List(
          Functor("a", List()),
          Variable("Z")
        )
      )
    )

    val answer = "foo(a, Z)"
    val result = Loprog.showValue(Variable("X"), bindings)

    assert(result === answer)
  }

  test("showValue (recursive)") {
    val bindings = Map(
      // X = Y
      Variable("X") -> Variable("Y"),

      // Y = foo(a, X)
      Variable("Y") -> Functor("foo",
        List(
          Functor("a", List()),
          Variable("X")
        )
      )
    )

    val answer = "foo(a, **)"
    val result = Loprog.showValue(Variable("X"), bindings)

    assert(result == answer)
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

    val result = Loprog.scopePredicate(predicate, generator)
    assert(result === answer)
  }
}
