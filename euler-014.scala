import collection.mutable.TreeSet
import collection.mutable.HashMap

object collatz {
  def collatz_counter(n: Int): Int = {
    // next collatz number
    def collatz(n: Long): Long = n match {
        case 1 => 0 
        case e if e % 2 == 0 => e / 2
        case o => 3 * o + 1
    }

    def iter(n: Long, c: Int): Int = {
      if (n.isValidInt && cache.get(n.toInt).isDefined) cache(n.toInt) + c
      else {
        val next = collatz(n)
        if (next == 0) c
        else iter (next, c + 1)
      }  
    }

    val ret = iter(n, 1)
    if (counter.size > 100000) {
      //val less = counter.init.last // to keep biggest one
      val last = counter.last
      counter -= last

      val less = counter.last
      counter -= less
      cache -= less._2

      counter += last
    }
    cache += (n -> ret)
    counter += (ret -> n)
    ret
  }

  // N, collatz(N)
  val cache: HashMap[Int, Int] = new HashMap[Int, Int]
  // counter, N
  val counter: TreeSet[(Int, Int)] = new TreeSet[(Int, Int)]
  def apply(n: Int) = collatz_counter(n)
}   

//1 to 20 foreach(n => println(s"$n => ${collatz(n)}"))
1 to 1000000 foreach(collatz(_))
println("ans: " + collatz.counter.last)