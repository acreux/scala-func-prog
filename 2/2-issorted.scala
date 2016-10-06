import scala.annotation.tailrec


// def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
//    as.zip(as.tail).forall { case (i, j) => ordered(i, j) }
//    as.zip(as.tail).forall(Function.tupled(ordered))
// }

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

	@tailrec
	def isSortedTail(as: Array[A], res: Boolean = true): Boolean = {
		res && (if (as.lengthCompare(2) < 0) true
			else isSortedTail(as.tail, ordered(as.head, as.tail.head))	
		)
	}
	
	isSortedTail(as)
}

def ord(i: Int, j: Int): Boolean = i < j

println(isSorted(Array(1, 2, 3, 4), ord)) 
println(isSorted(Array(1, 5, 3, 4), ord)) 
