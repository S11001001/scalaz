package scalaz
package std

trait PartialFunctionInstances {
  implicit val partialFunctionInstance: Arrow[PartialFunction] with Category[PartialFunction] with Choice[PartialFunction] = new Arrow[PartialFunction] with Category[PartialFunction] with Choice[PartialFunction] {
    import scala.{PartialFunction => PF}

    // as in object PartialFunction. We can't do any better than this,
    // because that's how applyOrElse is designed.
    val fallbackWitness: Any => Any = (a: Any) => fallbackWitness
    def fallback[A, B]: A => B = fallbackWitness.asInstanceOf[Any => B]
    def fallbackp(x: Any) = fallbackWitness eq x.asInstanceOf[AnyRef]

    def arr[A, B](f: A => B) = PF(f)
    def compose[A, B, C](f: PF[B, C], g: PF[A, B]) = new PF[A, C] {
      def apply(a: A): C = f(g(a))
      def isDefinedAt(a: A): Boolean = g.isDefinedAt(a) && f.isDefinedAt(g(a))
      override def applyOrElse[A1 <: A, B1 >: C](x: A1, default: A1 => B1): B1 = {
        val b = g.applyOrElse(x, fallback[A, B])
        if (fallbackp(b)) default(x) else f.applyOrElse(b, _ => default(x))
      }
    }

    def id[A] = {
      case a => a
    }

    def choice[A, B, C](f: => PF[A, C], g: => PF[B, C]): PF[A \/ B, C] = {
      case -\/(a) if f isDefinedAt a => f(a)
      case \/-(b) if g isDefinedAt b => g(b)
    }

    override def split[A, B, C, D](f: PF[A, B], g: PF[C, D]): PF[(A, C), (B, D)] = {
      case (a, c) if f.isDefinedAt(a) && g.isDefinedAt(c) => (f(a), g(c))
    }

    def first[A, B, C](f: PF[A, B]): PF[(A, C), (B, C)] = {
      case (a, c) if f.isDefinedAt(a) => (f(a), c)
    }

    def mapfst[A, B, C](fab: (A PF B))(f: C => A): (C PF B) = new (C PF B) {
      override def applyOrElse = ()
    }

    def mapsnd[A, B, C](fab: (A PF B))(f: B => C): (A PF C) =
      fab andThen f
  }
}

object partialFunction extends PartialFunctionInstances
