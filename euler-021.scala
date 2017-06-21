import scala.collection.mutable.{HashMap, HashSet}
object global {
    val divisors: HashMap[Int, HashSet[Int]] = new HashMap
    val d: HashMap[Int, Int] = new HashMap

    def getDivisors(m: Int): HashSet[Int] = {
        if (m <= 3) HashSet(1)
        else if (divisors.isDefinedAt(m)) divisors(m)
        else {
            // get divisors of (1 to m/2)
            val divs = (for {
                challenge <- (m / 2) to 1 by -1
                if (m % challenge == 0)
            } yield (getDivisors(challenge) + challenge))
            
            // divs: (HashSet, HashSet, HashSet) --> (HashSet)
            divisors += (m -> (divs.reduce(_ ++ _)))

            divisors(m)
        }
    }
}
import global._

// calculate d() for all numbers
(1 to 10000).foreach { n => 
    val sum = getDivisors(n).sum
    // ignore primes
    if (sum != 1) {
        d += (n -> sum)
    }
}

// test whether n == d(d(n))
val amicable = for {
    n <- d.keySet
    dn = d(n)
    if (d.isDefinedAt(dn))
    if (n != dn) // no perfect numbers
    if (n == d(dn))
} yield n

println("ans: " + amicable.sum)