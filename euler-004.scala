import scala.collection.mutable.ArrayBuffer;

// let's do it simple -
def isPalindrome(number: Long) = number.toString == number.toString.reverse

// p3's solution can be re-used again for solving p4
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

def isTwoOf100s(list: List[Long]): Boolean = {
  if (list.size == 2) {
    100 <= list(0) && list(0) <= 999 &&
    100 <= list(1) && list(1) <= 999
  } else if (list.head < 1000) {
    val head = list.head
    val tail = list.tail

    (0 until tail.size).exists { i =>
      val selected = head * tail(i)
      val remained = tail.take(i) ++ tail.drop(i + 1)
      //println(s"$list => $head * ${i+1}th[${tail(i)}], $remained")
      isTwoOf100s(selected :: remained)
    } || isTwoOf100s(List(head, tail.reduce(_*_)))
  } else false
}

/*
for {
  n <- 999*999 to 100*100 by -1
  if isPalindrome(n) 
  factors = factorize(n)
  if factors.last < 1000
} println(s"$n - $factors")
*/

val answer = (999*999 to 100*100 by -1).filter(isPalindrome(_)).find { n =>
  val factors = factorize(n)
  if (factors.last < 1000) isTwoOf100s(factors.toList)
  else false
}.get

println(s"answer: $answer.get => ${factorize(answer)}")
