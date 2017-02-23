def squareOfSum(x: Long) = ((x) * (x+1) / 2) * ((x) * (x+1) / 2)
def sumOfSqares(x: Long) = (x) * (x+1) * (2*x + 1) / 6
val ans = squareOfSum(100) - sumOfSqares(100)
println(ans)