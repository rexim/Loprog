import org.scalatest._
import org.scalatest.matchers._
import com.github.rexim.{
  LoprogContext,
  Predicate,
  Functor,
  Atom,
  Variable
}

class LoprogContextSpec extends FlatSpec with ShouldMatchers {
  "LoprogContext" should "support operation of unification" in {
    // p(X, f(Y), a)
    val left = Functor("p", List(
      Variable("X"),
      Functor("f", List(Variable("Y"))),
      Atom("a")
    ))

    // p(a, f(a), Y)
    val right = Functor("p", List(
      Atom("a"),
      Functor("f", List(Atom("a"))),
      Variable("Y")
    ))

    // ?- p(X, f(Y), a) = p(a, f(a), Y).
    // X = a, Y = a.
    LoprogContext.unify(left, right) should equal (Map("X" -> Atom("a"), "Y" -> Atom("a")))
  }
}
