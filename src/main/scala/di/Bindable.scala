package di

trait Bindable[A, I <: Tuple, O]:
  extension (bindable: A) def tupled: I => O

object Bindable:
  given [I <: Tuple, O] as Bindable[I => O, I, O]:
    extension (bindable: I => O) def tupled: I => O = bindable
  
  given [F, I <: Tuple, O](using tupledFunction: TupledFunction[F, I => O]) as Bindable[F, I, O]:
    extension (bindable: F) def tupled: I => O = tupledFunction.tupled(bindable)