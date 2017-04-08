val a = Array.ofDim[Long](21, 21)
a(0)(0) = 1

for (i <- 0 until 21; j <- 0 until 21) {
    if (i == 0 && j == 0) a(i)(j) = 1
    else if (i == 0) a(i)(j) = a(i)(j-1)
    else if (j == 0) a(i)(j) = a(i-1)(j)
    else a(i)(j) = a(i)(j-1) + a(i-1)(j)
}

println(s"ans: ${a.last.last}")