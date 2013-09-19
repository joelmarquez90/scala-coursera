package recfun

object session {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(110); 
  def balance1(chars: List[Char]): Boolean = {
    balance(chars, 0) == 0
  };System.out.println("""balance1: (chars: List[Char])Boolean""");$skip(294); 

  def balance(chars: List[Char], count: Int): Int = {
    if (!chars.isEmpty) {
      val newCount = chars.head.charValue() match {
        case '(' => 1
        case ')' => if (count > 0) -1 else -2
        case _ => 0
      }
      balance(chars.tail, count + newCount)
    } else count
  };System.out.println("""balance: (chars: List[Char], count: Int)Int""");$skip(26); val res$0 = 

	balance1(List('a','b'));System.out.println("""res0: Boolean = """ + $show(res$0));$skip(26); val res$1 = 
  balance1(")(()".toList);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(48); val res$2 = 
  balance1("(if (zero? x) max (/ 1 x))".toList);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(87); val res$3 = 
  balance1("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList);System.out.println("""res3: Boolean = """ + $show(res$3));$skip(25); val res$4 = 
  balance1(":-)".toList);System.out.println("""res4: Boolean = """ + $show(res$4));$skip(26); val res$5 = 
  balance1("())(".toList);System.out.println("""res5: Boolean = """ + $show(res$5));$skip(142); 

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  };System.out.println("""pascal: (c: Int, r: Int)Int""");$skip(16); val res$6 = 

  pascal(0, 5);System.out.println("""res6: Int = """ + $show(res$6));$skip(15); val res$7 = 
  pascal(1, 3);System.out.println("""res7: Int = """ + $show(res$7));$skip(15); val res$8 = 
  pascal(8, 2);System.out.println("""res8: Int = """ + $show(res$8));$skip(32); 

  val a = Array("a", "b", "c");System.out.println("""a  : Array[String] = """ + $show(a ));$skip(26); 
  val b = Array("x", "y");System.out.println("""b  : Array[String] = """ + $show(b ));$skip(40); val res$9 = 
  for (a_ <- a; b_ <- b) yield (a_, b_);System.out.println("""res9: Array[(String, String)] = """ + $show(res$9));$skip(57); val res$10 = 

  List(1, 2, 3).toSet[Int].subsets.map(_.toList).toList;System.out.println("""res10: List[List[Int]] = """ + $show(res$10));$skip(299); 

  def countChange(money: Int, coins: List[Int]): Int = {
    val list = coins.toSet[Int].subsets.map(_.toList).toList
    var count = 0
    list.foreach { l =>
      var accum = l.foldLeft(0)((accum, elem) => accum + elem)
      if (accum != 0 && money % accum == 0) count += 1
    }
    count
  };System.out.println("""countChange: (money: Int, coins: List[Int])Int""");$skip(31); val res$11 = 
  
  countChange(3, List(2,1));System.out.println("""res11: Int = """ + $show(res$11));$skip(74); val res$12 = 
  
  List(5,10,20,50,100,200,500).toSet[Int].subsets.map(_.toList).toList;System.out.println("""res12: List[List[Int]] = """ + $show(res$12))}
}
