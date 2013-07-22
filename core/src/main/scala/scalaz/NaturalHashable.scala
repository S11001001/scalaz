package scalaz

////
/**
 *
 */
////
trait NaturalHashable[F] extends Equal[F] { self =>
  ////

  // derived functions

  ////
  val naturalHashableSyntax = new scalaz.syntax.NaturalHashableSyntax[F] { def F = NaturalHashable.this }
}

object NaturalHashable {
  @inline def apply[F](implicit F: NaturalHashable[F]): NaturalHashable[F] = F

  ////

  ////
}
