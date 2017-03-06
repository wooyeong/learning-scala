// n -> odd numbers, p -> primes
val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(n =>
  primes.takeWhile(p => p * p <= n).forall(n % _ != 0))

primes.take(100).foreach(println)
println(primes.take(10001).last)
