package recfun

object session {
  def balance1(chars: List[Char]): Boolean = {
    balance(chars, 0) == 0
  }                                               //> balance1: (chars: List[Char])Boolean

  def balance(chars: List[Char], count: Int): Int = {
    if (!chars.isEmpty) {
      val newCount = chars.head.charValue() match {
        case '(' => 1
        case ')' => if (count > 0) -1 else -2
        case _ => 0
      }
      balance(chars.tail, count + newCount)
    } else count
  }                                               //> balance: (chars: List[Char], count: Int)Int

	balance1(List('a','b'))                   //> res0: Boolean = true
  balance1(")(()".toList)                         //> res1: Boolean = false
  balance1("(if (zero? x) max (/ 1 x))".toList)   //> res2: Boolean = true
  balance1("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
                                                  //> res3: Boolean = true
  balance1(":-)".toList)                          //> res4: Boolean = false
  balance1("())(".toList)                         //> res5: Boolean = false

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }                                               //> pascal: (c: Int, r: Int)Int

  pascal(0, 5)                                    //> res6: Int = 1
  pascal(1, 3)                                    //> res7: Int = 3
  pascal(8, 2)                                    //> res8: Int = 0

  val a = Array("a", "b", "c")                    //> a  : Array[String] = Array(a, b, c)
  val b = Array("x", "y")                         //> b  : Array[String] = Array(x, y)
  for (a_ <- a; b_ <- b) yield (a_, b_)           //> res9: Array[(String, String)] = Array((a,x), (a,y), (b,x), (b,y), (c,x), (c,
                                                  //| y))

  List(1, 2, 3).toSet[Int].subsets.map(_.toList).toList
                                                  //> res10: List[List[Int]] = List(List(), List(1), List(2), List(3), List(1, 2),
                                                  //|  List(1, 3), List(2, 3), List(1, 2, 3))

  def countChange(money: Int, coins: List[Int]): Int = {
    val list = coins.toSet[Int].subsets.map(_.toList).toList
    var count = 0
    list.foreach { l =>
      var accum = l.foldLeft(0)((accum, elem) => accum + elem)
      if (accum != 0 && money % accum == 0) count += 1
    }
    count
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  
  countChange(3, List(2,1))                       //> res11: Int = 2
  
  List(5,10,20,50,100,200,500).toSet[Int].subsets.map(_.toList).toList
                                                  //> res12: List[List[Int]] = List(List(), List(5), List(10), List(500), List(20
                                                  //| ), List(50), List(200), List(100), List(5, 10), List(5, 500), List(5, 20), 
                                                  //| List(5, 50), List(5, 200), List(5, 100), List(10, 500), List(10, 20), List(
                                                  //| 10, 50), List(10, 200), List(10, 100), List(500, 20), List(500, 50), List(5
                                                  //| 00, 200), List(500, 100), List(20, 50), List(20, 200), List(20, 100), List(
                                                  //| 50, 200), List(50, 100), List(200, 100), List(5, 10, 500), List(5, 10, 20),
                                                  //|  List(5, 10, 50), List(5, 10, 200), List(5, 10, 100), List(5, 500, 20), Lis
                                                  //| t(5, 500, 50), List(5, 500, 200), List(5, 500, 100), List(5, 20, 50), List(
                                                  //| 5, 20, 200), List(5, 20, 100), List(5, 50, 200), List(5, 50, 100), List(5, 
                                                  //| 200, 100), List(10, 500, 20), List(10, 500, 50), List(10, 500, 200), List(1
                                                  //| 0, 500, 100), List(10, 20, 50), List(10, 20, 200), List(10, 20, 100), List(
                                                  //| 10, 50, 200), List(10, 50, 100), List(10, 200, 100), List(500, 20, 50), Lis
                                                  //| t(500, 20, 200), List(5
                                                  //| Output exceeds cutoff limit.
}