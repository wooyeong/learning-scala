import scala.collection.mutable.ArrayBuffer;

val euler_003_challenge = 600851475143L

def factorize(number: Long) = {
  def divide(number: Long, primes: ArrayBuffer[Long])(divisor: Long): Long = {
    var target = number
    val isPrime = (testee: Long) => primes.exists(testee % _ == 0)
    //println(s"starting round> $target, by ${divisor}")

    if (isPrime(divisor)) divide(target, primes)(divisor + 2)
    else {
      primes += divisor

      while (target % divisor == 0) {
        //println(s"dividing $target by $divisor")
        target /= divisor
      }

      if (Math.sqrt(target) > divisor)
        divide(target, primes) {
          if (divisor == 2) 3
          else divisor + 2
        }
      else {
        //println (s"end> returning $divisor or $target, known primes: $primes")
        if (target == 1 || isPrime(target)) divisor
        else target
      }
    }
  }

  if (1 == number) number
  else divide(number, ArrayBuffer.empty[Long])(2)
}

// (1 to 100) foreach (n => println(s"$n => " + factorize(n)))
println("answer: "+ factorize(euler_003_challenge))
