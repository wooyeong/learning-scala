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

def sigma(x: Int) = x * (x+1) / 2


var n = 2
while (factorize(sigma(n)).groupBy(identity).values.map(_.size).foldLeft(1)((a, b) => a * (b+1)) < 500)
  n += 1
println("ans: " + sigma(n))

val stream = Stream.from(2)
val ans = stream.takeWhile(n => factorize(sigma(n)).groupBy(identity).values.map(_.size).foldLeft(1)((a, b) => a * (b+1)) < 500)
println("ans: " + sigma(ans.last + 1))

val ans2 = (1 to Integer.MAX_VALUE).find(n => factorize(sigma(n)).groupBy(identity).values.map(_.size).foldLeft(1)((a, b) => a * (b+1)) >= 500).get
println("ans: " + sigma(ans2))