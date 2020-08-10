
import org.junit.Test
import org.junit.Assert._
import scala.util._
import di._
import fp._
import fp.Id.{ given _ }

class Test1 {
  val idDesign = Design.of[Id]
      .bind(Foo.apply)
      .bind(Bar.apply)
      .bind(Baz.apply)
      .give(1)

  @Test def testResolve(): Unit = {
//     val resolved = idDesign.resolve[Baz *: EmptyTuple]
    // assertEquals(resolved, Baz(Bar(Foo(1)), 1) *: EmptyTuple)
  }

  @Test def testResolveAll(): Unit = {
    // val resolved = idDesign.resolveAll
    // assertEquals(resolved, 1 *: Baz(Bar(Foo(1)), 1) *: Bar(Foo(1)) *: Foo(1) *: EmptyTuple)
  }
}
