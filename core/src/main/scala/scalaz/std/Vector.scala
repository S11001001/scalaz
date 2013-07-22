package scalaz
package std

trait IndexedSeqSubVector extends IndexedSeqSub {
  type IxSq[+A] = Vector[A]
  protected final def buildIxSq[A, B] = implicitly
  protected final def covariant = vector.vectorInstance
  protected final def empty[A] = Vector()
}

sealed trait VectorInstances1 {
  implicit def vectorEqual[A](implicit A0: Equal[A]) = new IndexedSeqEqual[A, Vector[A]] {
    implicit def A = A0
  }
}

sealed trait VectorInstances0 extends VectorInstances1 {
  object generic extends IndexedSeqSubVector with IndexedSeqSubInstances

  implicit def vectorOrder[A](implicit A0: Order[A]): Order[Vector[A]] = generic.ixSqOrder
}

trait VectorInstances extends VectorInstances0 {
  implicit val vectorInstance = generic.ixSqInstance

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = generic.ixSqMonoid

  implicit def vectorShow[A: Show]: Show[Vector[A]] = generic.ixSqShow

  implicit def vectorNaturalHashable[A: NaturalHashable]: NaturalHashable[Vector[A]] =
    NaturalHashable.instance
}

object vector extends IndexedSeqSubVector with VectorInstances with IndexedSeqSubFunctions {
  object vectorSyntax extends scalaz.syntax.std.ToVectorOps
}
