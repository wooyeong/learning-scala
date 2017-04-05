var a = BigInt.int2bigInt(1)
for (n <- 1 to 1000) a = a * 2
val ans = a.toString.map(_.asDigit).sum
println(ans)