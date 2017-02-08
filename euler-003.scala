import scala.collection.mutable.ArrayBuffer;

val euler_003_challenge = 600851475143L

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

//(2 to 100) foreach (n => println(s"$n => " + factorize(n)))
println("answer: "+ factorize(euler_003_challenge))
