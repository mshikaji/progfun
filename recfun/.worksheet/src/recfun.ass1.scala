package recfun

object ass1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(54); 
  val l = List(1, 2, 3);System.out.println("""l  : List[Int] = """ + $show(l ));$skip(7); val res$0 = 
  l(2);System.out.println("""res0: Int = """ + $show(res$0));$skip(11); val res$1 = 
  l :+ 666;System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(131); 

  def f(l: List[Int]) =
    l.foldLeft(List[Int](1)) { (xs, x) =>
      if (x == 1) xs :+ x
      else xs :+ (xs.last + x)
    };System.out.println("""f: (l: List[Int])List[Int]""");$skip(13); val res$2 = 

	f(List(2));System.out.println("""res2: List[Int] = """ + $show(res$2))}
}
