package di
import scala.implicits.Not
import _root_.fp._

trait FindEntry[F[_], T <: Tuple, Out]:
  type In <: Tuple
  def apply(tuple: T): Entry[F, In, Out]

object FindEntry:
  type Aux[F[_], T <: Tuple, I <: Tuple, O] = FindEntry[F, T, O] { type In = I }

  given findEntryAtHead[F[_], I <: Tuple, O, Tail <: Tuple] as FindEntry[F, Entry[F, I, O] *: Tail, O]:
    type In = I
    def apply(tuple: Entry[F, In, O] *: Tail): Entry[F, I, O] = tuple.head
  
  given findEntryInTail[F[_], HeadIn <: Tuple, HeadOut, Tail <: Tuple, I <: Tuple, O](
    using headIsNotTheTarget: Not[HeadOut =:= O],
    findEntry: FindEntry.Aux[F, Tail, I, O]
  ) as FindEntry[F, Entry[F, HeadIn, HeadOut] *: Tail, O]:
    // NOTE: For some reason, findEntry.In can't be assigned here. Using Aux pattern instead.
    type In = I
    def apply(tuple: Entry[F, HeadIn, HeadOut] *: Tail): Entry[F, I, O] = findEntry(tuple.tail)

extension [F[_]: Monad, I <: Tuple, O](entry: Entry[F, I, O]):
  def memoize(in: I): StateT[F, Memo, O] = StateT.run(memo => {
    memo.get(entry) match {
      case Some(memoized) => (memo, memoized.asInstanceOf[O]).pure
      case None => entry(in).map(out => (memo.updated(entry, out), out))
    }
  })