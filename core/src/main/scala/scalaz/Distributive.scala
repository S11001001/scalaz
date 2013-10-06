package scalaz

/** Dual of [[scalaz.Traverse]].  To transform `F[G[B]]` to `G[F[B]]`,
  * you may use `Traverse[F]` and `Applicative[G]`, but alternatively
  * `Functor[F]` and `Distributive[G]`, which permits greater sharing
  * and nonstrictness.
  *
  * By replacing `Applicative` with `Distributive`, you gain the
  * ability to "pull out" the distributive functor from ''any'' `G`
  * functor, not just traversables.  However, this means that the `F`
  * structure cannot depend on the `F` structures produced within the
  * functor; the distribution must produce one structure guaranteed to
  * be able to substitute all inner `F` structures for values of their
  * type parameters, at any degree of strictness chosen by the `G`.
  *
  * Accordingly, very few functors are distributive.  The only common
  * example is the function, which is distributive over its result, as
  * characterized by [[scalaz.Functor]]`#mapply`.  [[scalaz.Need]] and
  * [[scalaz.Name]] are other useful distributive functors, but they
  * are more or less glorified functions.
  *
  * [[scalaz.effect.IO]] is '''not''' distributive, even with liberal
  * application of `unsafePerformIO`, because it is sensitive to
  * `G`-strictness.  For the same reason, similar effect models also
  * cannot be considered distributive, even when Scala's uncontrolled
  * side effects lets them pass type-checking.
  *
  * @note No laws are defined for Distributive, because none are
  *       needed; all of the above derives from parametricity of the
  *       primitive `distribute` operation.
  */
trait Distributive[F[_]] extends Functor[F] { self =>
  /** Transform ''fa'' using ''f'', running all the `F`s on-demand in
    * the outer `F`-context.
    *
    * @note Call `distribute` instead as a user.
    */
  def distributeImpl[G[_]:Functor,A,B](fa: G[A])(f: A => F[B]): F[G[B]]


  /** The composition of Distributives `F` and `G`, `[x]F[G[x]]`, is a
    * Distributive.
    *
    * @note `ff.compose(fg).distribute(ha)(f)` = `ha.distribute(f).map(_.cosequence)` */
  def compose[G[_]](implicit G0: Distributive[G]): Distributive[({type λ[α] = F[G[α]]})#λ] = new CompositionDistributive[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Distributives `F` and `G`, `[x](F[x], G[x]])`, is a Distributive */
  def product[G[_]](implicit G0: Distributive[G]): Distributive[({type λ[α] = (F[α], G[α])})#λ] = new ProductDistributive[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  class Distribution[G[_]](implicit G: Functor[G]) {
    def run[X[_]:Functor,A,B](fa: X[A])(f: A => F[B]): F[X[B]] = distributeImpl(fa)(f)
  }

  def distribution[G[_]:Functor]: Distribution[G] =
    new Distribution[G]

  /** Transform ''fa'' using ''f'', running all the `F`s on-demand in
    * the outer `F`-context.
    *
    * @note Syntax is defined on [[scalaz.syntax.FunctorOps]], because
    *       the operand ''fa'' is the functorial, not the
    *       distributive.
    */
  def distribute[G[_]:Functor,A,B](fa: G[A])(f: A => F[B]): F[G[B]] =
    distribution[G].run(fa)(f)

  /** Distribute with the identity function.
    *
    * @note `ga.map(f).cosequence` = `gfa.distribute(f)` */
  def cosequence[G[_]:Functor,A](fa: G[F[A]]): F[G[A]] =
    distributeImpl(fa)(x => x)

}

object Distributive extends DistributiveFunctions {
  @inline def apply[F[_]](implicit F: Distributive[F]): Distributive[F] = F

}
trait DistributiveFunctions {
  // Distributive is the dual of Traverse.
  type Cotraverse[F[_]] =
  Distributive[F]
}
