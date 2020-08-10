import scala.util._
import di._

case class Foo(value: Int)
case class Bar(foo: Foo)
case class Baz(bar: Bar, value: Int)

val baseDesign = Design.of[Try]
  .bind(Foo.apply)
  .bind(Bar.apply)
  .bindF((bar: Bar, value: Int) => (
    if (value >= 0) Success(Baz(bar, value))
    else Failure(new Exception(s"value should be positive (got ${value}).")
  )))

baseDesign.resolveAll.toString

val designWithOne = baseDesign.give(1)
designWithOne.resolveAll.toString

val designWithMinusOne = baseDesign.give(-1)
designWithMinusOne.resolveAll.toString
