package scalaz

import scalacheck.ScalazProperties

class FoldableTest extends Spec {
  private val L = Foldable[List]

  "product foldRight equivalence" ! prop {
    (l: List[Int], l2: List[Int]) =>
      L.product(L).foldRight((l, l2), List.empty[Int])(_ :: _) must be_===(l ++ l2)
  }

  "product foldLeft equivalence" ! prop {
    (l: List[Int], l2: List[Int]) =>
      (L.product(L).foldLeft((l, l2), List.empty[Int])((xs, x) => x :: xs)
       must be_===((l ++ l2).reverse))
  }
}
