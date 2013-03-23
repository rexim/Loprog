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
    Loprog.unify(left, right, Map()) match {
      case Some(bindings) => {
        assert(bindings.get("X") === Some(Atom("a")))
        assert(bindings.get("Y") === Some(Atom("a")))
      }
      case None => assert(false, s"$left doesn't unify $right")
    }
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
    // X = _G101; Z = _G101; Y = b.
    Loprog.unify(left, right, Map()) match {
      case Some(bindings) => {
        assert(bindings.get("Y") === Some(Atom("b")))
        assert(bindings.get("X") === bindings.get("Z"))
        bindings.get("X") match {
          case Some(Variable(varName)) =>
            assert(!bindings.contains(varName), "X refers to a bound variable")
          case _ =>
            assert(false, "X doesn't refer to a variable")
        }
      }
      case None => assert(false, s"$left doesn't unify $right")
    }
  }
}
