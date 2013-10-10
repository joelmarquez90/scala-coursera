object intsets {
  val t5 = new NonEmpty(5, new NonEmpty(3, Empty, Empty), new NonEmpty(7, Empty, Empty))
                                                  //> t5  : NonEmpty = {{.3.}5{.7.}}
  val t2 = new NonEmpty(2, Empty, Empty)          //> t2  : NonEmpty = {.2.}
  val t4 = new NonEmpty(4, Empty, Empty)          //> t4  : NonEmpty = {.4.}
  val t6 = new NonEmpty(6, Empty, Empty)          //> t6  : NonEmpty = {.6.}
  val t8 = new NonEmpty(8, Empty, Empty)          //> t8  : NonEmpty = {.8.}
  val t87 = new NonEmpty(8, new NonEmpty(7, Empty, Empty), Empty)
                                                  //> t87  : NonEmpty = {{.7.}8.}
  val unionT5T2 = t5 union t2                     //> unionT5T2  : IntSet = {.2{.3{{.5.}7.}}}
  val unionT5T4 = t5 union t4                     //> unionT5T4  : IntSet = {{.3.}4{{.5.}7.}}
  val unionT5T6 = t5 union t6                     //> unionT5T6  : IntSet = {{.3{.5.}}6{.7.}}
  val unionT5T8 = t5 union t8                     //> unionT5T8  : IntSet = {{.3{{.5.}7.}}8.}
  val unionT5T87 = t5 union t87                   //> unionT5T87  : IntSet = {{{.3{.5.}}7.}8.}
}

abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

object Empty extends IntSet {
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other
	override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def incl(x: Int): IntSet =
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	def contains(x: Int): Boolean =
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
	def union(other: IntSet): IntSet =
		((left union right) union other) incl elem
		
	override def toString = "{" + left + elem + right + "}"
}