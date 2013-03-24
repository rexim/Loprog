import org.scalatest.FunSuite
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
}
