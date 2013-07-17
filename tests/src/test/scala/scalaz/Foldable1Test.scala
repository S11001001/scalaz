package scalaz

import scalacheck.ScalazProperties

class Foldable1Test extends Spec {
  private val L = Foldable1[List]

  "product foldRight1 equivalence" ! prop {
    (l: NonEmptyList[List[Int]], l2: NonEmptyList[List[Int]]) =>
      (L.product(L).foldRight1((l, l2))(_ ++ _)
       must be_===((l.list ++ l2.list).flatten))
  }

  "product foldLeft1 equivalence" ! prop {
    (l: List[Int], l2: List[Int]) =>
      (L.product(L).foldLeft((l, l2), List.empty[Int])((xs, x) => x ++ xs)
       must be_===((l.list ++ l2.list).reverse.flatten))
  }
}
