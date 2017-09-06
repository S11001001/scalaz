package scalaz
package data

import Prelude._
import typeclass.IsCovariant._
import typeclass.IsCovariantClass
import typeclass.Liskov.<~<

trait IListModule {
  type IList[A]

  def uncons[A](as: IList[A]): Option[(A, IList[A])]

  implicit def isCovariantInstance: IsCovariant[IList]
}

private[data] trait IListImpl {
  type Option2[+A, +B] = Option[(A, B)]
  type IList[A] = Fix[Option2[A, ?]]

  def uncons[A](as: IList[A]): Option[(A, IList[A])] =
    Fix.unfix[Option2[A, ?]](as)

  implicit val isCovariantInstance: IsCovariant[IList] =
    new IsCovariantClass[IList] with IsCovariantClass.LiftLiskov[IList] {

      override def liftLiskov[A, B](implicit ev: A <~< B): IList[A] <~< IList[B] = {
        type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
        val ev1 = Λ[α](scalaCovariant[Option2[+?, α]].liftLiskov[A, B]): Option2[A, ?] <~~< Option2[B, ?]
        Fix.liftLiskov[Option2[A, ?], Option2[B, ?]](ev1.make)(scalaCovariant[Option2[A, +?]])
      }
    }
}
