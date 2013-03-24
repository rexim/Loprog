import org.scalatest.FunSuite
import scala.collection.mutable.ListBuffer
import com.github.rexim._

class LoprogSuite extends FunSuite {
  test("unify: simple unification") {
    // p(X, f(Y), a)
    val left = Functor("p",
      List(
        Variable("X"),
        Functor("f", List(Variable("Y"))),
        Atom("a")
      )
    )

    // p(a, f(a), Y)
    val right = Functor("p",
      List(
        Atom("a"),
        Functor("f", List(Atom("a"))),
        Variable("Y")
      )
    )

    // ?- p(X, f(Y), a) = p(a, f(a), Y).
    // X = a; Y = a.
    val bindings = Loprog.unify(left, right, Map())
    assert(bindings === Some(Map("X" -> Atom("a"), "Y" -> Atom("a"))))
  }

  test("unify: unification with shared references") {
    // p(X, f(Y), a)
    val left = Functor("p",
      List(
        Variable("X"),
        Functor("f", List(Variable("Y"))),
        Atom("a")
      )
    )

    // p(Z, f(b), a)
    val right = Functor("p",
      List(
        Variable("Z"),
        Functor("f", List(Atom("b"))),
        Atom("a")
      )
    )

    // ?- p(X, f(Y), a) = p(Z, f(b), a).
    // X = Z; Y = b.
    Loprog.unify(left, right, Map()) match {
      case Some(bindings) => {
        assert(bindings === Map("X" -> Variable("Z"), "Y" -> Atom("b")))
        // FIXME: omg, make it shorter.
        assert(Loprog.unify(Variable("X"), Atom("d"), bindings) === Some(Map("X" -> Variable("Z"), "Y" -> Atom("b"), "Z" -> Atom("d"))))
      }

      case None =>
        assert(false, s"$left doesn't unify $right")
    }
  }

  test("visitSolutions: a simple query on a program.") {
    val program = List(
      // p(a).
      Predicate(Functor("p", List(Atom("a"))), List()),

      // p(X) :- q(X), r(X).
      Predicate(Functor("p", List(Variable("p1::X"))),
        List(
          Functor("q", List(Variable("p1::X"))),
          Functor("r", List(Variable("p1::X")))
        )),

      // p(X) :- u(X).
      Predicate(Functor("p", List(Variable("p2::X"))),
        List(
          Functor("u", List(Variable("p2::X")))
        )),

      // q(X) :- s(X).
      Predicate(Functor("q", List(Variable("q1::X"))),
        List(
          Functor("s", List(Variable("q1::X")))
        )),

      // r(a).
      Predicate(Functor("r", List(Atom("a"))), List()),

      // r(b).
      Predicate(Functor("r", List(Atom("b"))), List()),

      // s(a).
      Predicate(Functor("s", List(Atom("a"))), List()),

      // s(b).
      Predicate(Functor("s", List(Atom("b"))), List()),

      // s(c).
      Predicate(Functor("s", List(Atom("c"))), List()),

      // u(d).
      Predicate(Functor("u", List(Atom("d"))), List())
    )

    // ?- p(X).
    val query = List(Functor("p", List(Variable("X"))))

    val solutions = new ListBuffer[Map[String, Term]]()

    Loprog.visitSolutions(program, query,
      (bindings: Map[String, Term]) => {
        solutions.append(bindings)
      }, Map())

    assert(solutions.toList.map(_.get("X")) == List(Some(Atom("a")), Some(Atom("a")), Some(Atom("b")), Some(Atom("d"))))
  }
}
