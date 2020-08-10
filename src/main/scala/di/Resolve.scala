package di
import _root_.fp._
type Resolved[F[_], A] = StateT[F, Memo, A]

trait Resolve[F[_], Entries <: Tuple, Out <: Tuple]:
  def apply(entries: Entries): Resolved[F, Out]

object Resolve:
  given resolveEmptyTuple[F[_]: Monad, Entries <: Tuple] as Resolve[F, Entries, EmptyTuple]:
    def apply(entries: Entries): Resolved[F, EmptyTuple] = StateT.pure(EmptyTuple)
    
  given resolveNonEmptyTuple[F[_]: Monad, Entries <: Tuple, Head, Tail <: Tuple](
    using findEntry: FindEntry[F, Entries, Head],
    in: Resolve[F, Entries, findEntry.In],
    tail: Resolve[F, Entries, Tail]
  ) as Resolve[F, Entries, Head *: Tail]:
    def apply(entries: Entries): Resolved[F, Head *: Tail] =
      for {
        i <- in(entries)
        h <- findEntry(entries).memoize(i)
        t <- tail(entries)
      } yield h *: t
