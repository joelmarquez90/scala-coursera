package week2

object session {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum(x => x * x, 3, 5)                           //> res0: Int = 50

  def singletonSet(elem: Int): Int => Boolean = x => x == elem
                                                  //> singletonSet: (elem: Int)Int => Boolean
	singletonSet(80)                          //> res1: Int => Boolean = <function1>
}