package di
import scala.Tuple._
import scala.implicits.Not
import _root_.fp._

type Entry[F[_], In <: Tuple, Out] = In => F[Out]

type Memo = scala.Predef.Map[Any, Any]
object Memo:
  val empty: Memo = Map.empty

class Design[F[_]: Monad, Entries <: Tuple] private (val entries: Entries):

  type Outputs[A <: Tuple] = Map[A, [X] =>> X match {
    case Entry[F, _, out] => out
  }]

  def give[A](value: A): Design[F, Entry[F, EmptyTuple, A] *: Entries] =
    bind((et: EmptyTuple) => value)

  def bind[G, I <: Tuple, O](f: G)(using bindable: Bindable[G, I, O]): Design[F, Entry[F, I, O] *: Entries] =
    Design(f.tupled.andThen(_.pure) *: entries)

  def bindF[G, I <: Tuple, O](f: G)(using bindable: Bindable[G, I, F[O]]): Design[F, Entry[F, I, O] *: Entries] =
    Design(f.tupled *: entries)

  def resolve[O <: Tuple](using r: Resolve[O]): F[O] = r(entries).run(Memo.empty).map(_._2)

  def resolveAll(using Resolve[Outputs[Entries]]): F[Outputs[Entries]] = resolve[Outputs[Entries]]

  type Resolved[A] = StateT[F, Memo, A]
  
  trait Resolve[Out <: Tuple]:
    def apply(entries: Entries): Resolved[Out]
  
  object Resolve:
    given resolveEmptyTuple as Resolve[EmptyTuple]:
      def apply(entries: Entries): Resolved[EmptyTuple] = StateT.pure(EmptyTuple)
      
    given resolveNonEmptyTuple[Head, Tail <: Tuple](
      using
      tail: Resolve[Tail],
      findEntry: FindEntry[Entries, Head],
      in: Resolve[findEntry.In]
    ) as Resolve[Head *: Tail]:
      def apply(entries: Entries): Resolved[Head *: Tail] =
        for {
          i <- in(entries)
          h <- findEntry(entries).memoize(i)
          t <- tail(entries)
        } yield h *: t

  trait FindEntry[T <: Tuple, Out]:
    type In <: Tuple
    def apply(tuple: T): Entry[F, In, Out]
  
  object FindEntry:
    type Aux[T <: Tuple, I <: Tuple, O] = FindEntry[T, O] { type In = I }
  
    given findEntryAtHead[I <: Tuple, O, Tail <: Tuple] as FindEntry[Entry[F, I, O] *: Tail, O]:
      type In = I
      def apply(tuple: Entry[F, In, O] *: Tail): Entry[F, I, O] = tuple.head
  
    given findEntryInTail[HeadIn <: Tuple, HeadOut, Tail <: Tuple, I <: Tuple, O](
      using headIsNotTheTarget: Not[HeadOut =:= O],
      findEntry: FindEntry.Aux[Tail, I, O]
    ) as FindEntry[Entry[F, HeadIn, HeadOut] *: Tail, O]:
      // NOTE: For some reason, findEntry.In can't be assigned here. Using Aux pattern instead.
      type In = I
      def apply(tuple: Entry[F, HeadIn, HeadOut] *: Tail): Entry[F, I, O] = findEntry(tuple.tail)


object Design:
  def of[F[_]: Monad]: Design[F, EmptyTuple] = Design(EmptyTuple)

extension [F[_]: Monad, I <: Tuple, O](entry: Entry[F, I, O]):
  def memoize(in: I): StateT[F, Memo, O] = StateT.run(memo => {
    memo.get(entry) match {
      case Some(memoized) => (memo, memoized.asInstanceOf[O]).pure
      case None => entry(in).map(out => (memo.updated(entry, out), out))
    }
  })