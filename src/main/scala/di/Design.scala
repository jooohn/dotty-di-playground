package di
import scala.Tuple._
import scala.implicits.Not
import _root_.fp._

type Entry[F[_], In <: Tuple, Out] = In => F[Out]

type Memo = scala.Predef.Map[Any, Any]
object Memo:
  val empty: Memo = Map.empty

class Design[F[_]: Monad, Entries <: Tuple] private (val entries: Entries):

  type Output[A] = A match { case Entry[F, _, out] => out }
  type Outputs[A <: Tuple] = Map[A, Output]

  def give[A](value: A): Design[F, Entry[F, EmptyTuple, A] *: Entries] =
    bind((et: EmptyTuple) => value)

  def bind[G, I <: Tuple, O](f: G)(using bindable: Bindable[G, I, O]): Design[F, Entry[F, I, O] *: Entries] =
    Design(f.tupled.andThen(_.pure) *: entries)

  def bindF[G, I <: Tuple, O](f: G)(using bindable: Bindable[G, I, F[O]]): Design[F, Entry[F, I, O] *: Entries] =
    Design(f.tupled *: entries)

  def resolve[O <: Tuple](using r: Resolve[F, Entries, O]): F[O] = r(entries).run(Memo.empty).map(_._2)

  def resolveAll(using Resolve[F, Entries, Outputs[Entries]]): F[Outputs[Entries]] = resolve[Outputs[Entries]]


object Design:
  def of[F[_]: Monad]: Design[F, EmptyTuple] = Design(EmptyTuple)