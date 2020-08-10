import scala.Tuple._
import scala.util._
import di.Design

object Main:
  def main(args: Array[String]): Unit =
    val design = Design.of[Try]
      .bind(Foo.apply)
      .bind(Bar.apply)
      .bindF((bar: Bar, value: Int) => (
        if (value >= 0) Success(Baz(bar, value))
        else Failure(new Exception(s"value should be positive (got ${value}).")
      )))
      .give(1)
    println(design.resolveAll)

case class Foo(value: Int)
case class Bar(foo: Foo)
case class Baz(bar: Bar, value: Int)