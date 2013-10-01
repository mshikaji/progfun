package recfun

object ass1 {
  val l = List(1, 2, 3)                           //> l  : List[Int] = List(1, 2, 3)
  l(2)                                            //> res0: Int = 3
  l :+ 666                                        //> res1: List[Int] = List(1, 2, 3, 666)

  def f(l: List[Int]) =
    l.foldLeft(List[Int](1)) { (xs, x) =>
      if (x == 1) xs :+ x
      else xs :+ (xs.last + x)
    }                                             //> f: (l: List[Int])List[Int]

	f(List(2))                                //> res2: List[Int] = List(1, 3)
}