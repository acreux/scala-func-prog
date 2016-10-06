
def fib(n: Int): Int = {

	@annotation.tailrec
	def fibTailRec(n: Int, a: Int = 1, b: Int = 0): Int = {
	    if (n == 0) 0
            else if (n == 1) a
	    else fibTailRec(n-1, a + b, a)
	}

	fibTailRec(n)
} 

println(fib(2))
println(fib(3))
println(fib(4))
println(fib(5))
