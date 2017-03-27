// n -> odd numbers, p -> primes
val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(n =>
  primes.takeWhile(p => p * p <= n).forall(n % _ != 0))

println(primes.takeWhile(_ <= 2000000).foldLeft(0L)((a, b) => a+b))
println(primes.takeWhile(_ <= 2000000).map(_.toLong).sum)