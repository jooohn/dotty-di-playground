package fp
import scala.util._

trait Monad[F[_]]:
  self =>
  def pure[A](value: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f andThen pure)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  extension [A](a: A):
    def pure: F[A] = self.pure(a)

  extension [A, B](fa: F[A]):
    def map(f: A => B): F[B] = self.map(fa)(f)
    def flatMap(f: A => F[B]): F[B] = self.flatMap(fa)(f)

object Monad:
  given Monad[Option]:
    def pure[A](value: A): Option[A] = Some(value)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  
  given Monad[Try]:
    def pure[A](value: A): Try[A] = Success(value)
    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
  
  given [Left] as Monad[[Right] =>> Either[Left, Right]]:
    def pure[A](value: A): Either[Left, A] = Right(value)
    def flatMap[A, B](fa: Either[Left, A])(f: A => Either[Left, B]): Either[Left, B] = fa.flatMap(f)

type Id[A] = A
object Id:
  given Monad[Id]:
    def pure[A](value: A): Id[A] = value
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

case class StateT[F[_]: Monad, S, A](runF: F[S => F[(S, A)]]):
  def run(initial: S): F[(S, A)] = runF.flatMap(r => r(initial))
  def map[B](f: A => B): StateT[F, S, B] = flatMap(f andThen StateT.pure)
  def flatMap[B](f: A => StateT[F, S, B]): StateT[F, S, B] =
    StateT(runF.flatMap(r => ((s1: S) => r(s1).flatMap((s2, a) => f(a).run(s2))).pure))

object StateT:
  def pure[F[_]: Monad, S, A](value: A): StateT[F, S, A] = StateT(((s: S) => (s, value).pure).pure)
  def run[F[_]: Monad, S, A](run: S => F[(S, A)]): StateT[F, S, A] = StateT(run.pure)

  given [F[_]: Monad, S] as Monad[[X] =>> StateT[F, S, X]]:
    def pure[A](value: A): StateT[F, S, A] = StateT.pure(value)
    def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
      fa.flatMap(f)

type State[S, A] = StateT[Id, S, A]