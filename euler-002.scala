object Fib {
  private val elem = scala.collection.mutable.ArrayBuffer.empty[Int] += 1 += 1

  def apply(n: Int): Int = {
    if (elem.size > n) elem(n)
    else build(n)
  }

  private def build(n: Int): Int = {
    for (i <- elem.size to n) elem += elem(i-2) + elem(i-1)
    apply(n)
  }
}

// debug
//(1 to 30).foreach(n => println(n + ": " + Fib(n)))

val stream = for (x <- Stream from 1; if (Fib(x) % 2 == 0)) yield Fib(x)
val next = stream.takeWhile(_ <= 4 * 1000 * 1000).toList 
//println(next)
println(next.sum)
