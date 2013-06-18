package jp.seraphr.tapl.utils

import language.higherKinds
import language.implicitConversions
import scala.util.Try
import scala.util.Success

trait Applicative[F[_]] {
  def fmap[A, B](f: A => B)(aFunctor: F[A]): F[B]
  def point[A](a: => A): F[A]
  def apply[A, B](f: F[A => B])(aFunctor: F[A]): F[B]
}

object Applicative {

  implicit class ApplyUtil[A, B, F[_]: Applicative](f: F[A => B]) {
    def <*>(aFunctor: F[A]): F[B] = implicitly[Applicative[F]].apply(f)(aFunctor)
  }

  implicit class PointApplyUtil[A, B, F[_]: Applicative](f: A => B) {
    private val mApp = implicitly[Applicative[F]]

    def <|>(aFunctor: F[A]): F[B] = mApp.apply(mApp.point(f))(aFunctor)
  }

  implicit def tupleToBuilder1[A, F[_]: Applicative](a: F[A]) = new Builder1(a)
  implicit def tupleToBuilder2[A, B, F[_]: Applicative](a: (F[A], F[B])) = a match { case (tA, tB) => new Builder2(tA, tB) }
  implicit def tupleToBuilder3[A, B, C, F[_]: Applicative](a: (F[A], F[B], F[C])) = a match { case (tA, tB, tC) => new Builder3(tA, tB, tC) }

  class Builder1[A1, F[_]: Applicative](aArg: F[A1]) {
    def apply[R](f: A1 => R) = f <|> aArg
  }

  class Builder2[A1, A2, F[_]: Applicative](aArg1: F[A1], Arg2: F[A2]) {
    def apply[R](f: (A1, A2) => R) = f.curried <|> aArg1 <*> Arg2
  }

  class Builder3[A1, A2, A3, F[_]: Applicative](aArg1: F[A1], aArg2: F[A2], aArg3: F[A3]) {
    def apply[R](f: (A1, A2, A3) => R) = f.curried <|> aArg1 <*> aArg2 <*> aArg3
  }
}

object ApplicativeInstances {
  implicit object OptionIsApplicative extends Applicative[Option] {
    override def point[A](a: => A) = Option(a)
    override def apply[A, B](f: Option[A => B])(aFunctor: Option[A]): Option[B] = {
      for {
        tF <- f
        tValue <- aFunctor
      } yield tF(tValue)
    }
    override def fmap[A, B](f: A => B)(aFunctor: Option[A]) = aFunctor.map(f)
  }

  implicit object TryIsApplicative extends Applicative[Try]{
    override def point[A](a: => A) = Success(a)
    override def apply[A, B](f: Try[A => B])(aFunctor: Try[A]): Try[B] = {
      for {
        tF <- f
        tValue <- aFunctor
      } yield tF(tValue)
    }
    override def fmap[A, B](f: A => B)(aFunctor: Try[A]) = aFunctor.map(f)
  }
}


