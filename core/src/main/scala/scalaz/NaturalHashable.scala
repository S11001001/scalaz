package scalaz

////
/**
 * Marker for types for which `Any.##`, and, by implication, `Any.==`,
 * are well-defined.
 */
////
trait NaturalHashable[F] extends Equal[F] { self =>
  ////

  final def equal(a1: F, a2: F) = a1 == a2

  override final def equalIsNatural = true

  // derived functions

  ////
  val naturalHashableSyntax = new scalaz.syntax.NaturalHashableSyntax[F] { def F = NaturalHashable.this }
}

object NaturalHashable {
  @inline def apply[F](implicit F: NaturalHashable[F]): NaturalHashable[F] = F

  ////
  private[this] def iinstance = new NaturalHashable[Any]{}

  /** Assert that `A` is naturally hashable. */
  def instance[A]: NaturalHashable[A] =
    iinstance.asInstanceOf[NaturalHashable[A]]
  ////
}
