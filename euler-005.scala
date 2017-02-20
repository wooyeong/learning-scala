import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.Map;

def factorize(number: Long) = {
  def divide(number: Long, primes: ArrayBuffer[Long]): ArrayBuffer[Long] = {
    var target = number
    val isPrime = (testee: Long) => primes.forall(testee % _ != 0)
    val factors = ArrayBuffer.empty[Long]

    val divisor = if (primes == Nil) 2L else if (primes.last == 2) 3L
        else (primes.last + 2 to math.sqrt(target).toLong by 2).find(isPrime(_)).getOrElse(0L)

    if (divisor == 0) factors += target
    else {
      //println(s"starting round> $target, by ${divisor}")
      primes += divisor

      while (target % divisor == 0) {
        target /= divisor
        factors += divisor
      }

      if (math.sqrt(target).toLong > divisor) factors ++= divide(target, primes)
      else {
        //println (s"end> returning $target?, known primes: $primes")
        if (target == 1) factors
        else factors += target
      }
    }
  }

  divide(number, ArrayBuffer.empty[Long])
}

val factors = Map.empty[Long, Int]
// seq of (factors, count)
(2 to 20) foreach (factorize(_).groupBy(identity).mapValues(_.size) foreach {
  case (k, v) => if (factors.getOrElse(k, 0) < v) factors(k) = v })
println (factors)

val ans = factors.foldLeft(1L)((mul, map) => mul * math.pow(map._1, map._2).toLong)
println(ans)
