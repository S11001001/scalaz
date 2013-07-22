package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `NaturalHashable` */
sealed abstract class NaturalHashableOps[F] extends Ops[F] {
  implicit def F: NaturalHashable[F]
  ////

  ////
}

trait ToNaturalHashableOps extends ToEqualOps {
  implicit def ToNaturalHashableOps[F](v: F)(implicit F0: NaturalHashable[F]) =
    new NaturalHashableOps[F] { def self = v; implicit def F: NaturalHashable[F] = F0 }

  ////

  ////
}

trait NaturalHashableSyntax[F] extends EqualSyntax[F] {
  implicit def ToNaturalHashableOps(v: F): NaturalHashableOps[F] = new NaturalHashableOps[F] { def self = v; implicit def F: NaturalHashable[F] = NaturalHashableSyntax.this.F }
  
  def F: NaturalHashable[F]
  ////

  ////
}
