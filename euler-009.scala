
def pythagorean(a: Int, b: Int) = math.sqrt(a*a + b*b)

val ans = for {
  a <- 1 to 1000
  b <- (a+1) to 1000
  c = pythagorean(a, b)
  // c must be an integer, according to the problem
  if (c.isValidInt)
  if (a + b + c == 1000)
} yield List(a, b, c.toInt)

println(ans);
println(ans.head.product);